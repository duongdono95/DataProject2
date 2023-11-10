options(max.print = 10000)

# install relevant packages
install.packages("tidyverse")
install.packages("magrittr")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("plotly")
install.packages("tidyr")
install.packages("purrr")
install.packages("cluster")
install.packages("factoextra")

# load the packages
library(tidyverse)
library(magrittr)
library(ggplot2)
library(dplyr)
library(rpart)
library(rpart.plot)
library(plotly)
library(tidyr)
library(purrr)
library(cluster)
library(factoextra) 

###################################################################################################################
######### 1. Data Preparation ###########

# import the data
data <- read.csv("./fastfood.csv")
print(data)

## Preparing the data
# check all the types of all columns
str(data)
print(str(data))

var_types <- sapply(data, class)
print(var_types)

grouped_vars <- split(names(data), var_types)
print(grouped_vars)

#using ANOVA method to see if there are any statistically significant differences between the means of calories across different restaurants.
anova_result <- aov(calories ~ restaurant, data = data)
print (anova_result)
summary(anova_result)
TukeyHSD(anova_result)

# categories the column titles into different types.
for (type in names(grouped_vars)) {
  cat(type, ":\n")
  cat(paste(grouped_vars[[type]], collapse = ", "), "\n\n")
}

# list out all the restaurant names
restaurant_names <- unique(data$restaurant)
print(restaurant_names)
# the dataset is the record of nutritional dishes in 8 restaurants: Mcdonals, chick Fil-A, Sonic, Arbys, Burger King, Dairy Queen, Subway, Taco Bell

# count the listed items per each restaurant - menu diversity
fastfood_counted_items <- data %>%
  group_by(restaurant) %>%
  count(restaurant, name= 'itemCount')
print(fastfood_counted_items)
ggplot(fastfood_counted_items, aes(x = reorder(restaurant, -itemCount), y = itemCount)) +
  geom_bar(stat = "identity", fill = 'pink') +
  labs(x = "Restaurant", y = "Number of Items", title = "Menu Diversity per Restaurant") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

 

# Calculate the mean calories
mean_calories <- mean(data$calories, na.rm = TRUE)

# Add a column that shows the absolute difference from the mean
top_three_add_column_data <- mutate(data, diff_from_mean = abs(calories - mean_calories))

# Concatenate the dish name and restaurant into a new column for labeling
top_three_concentrated_data <- mutate(top_three_add_column_data, label = paste(restaurant, "-", item))

# Find the top 3 foods with the most calories
top_most_calories <- top_three_concentrated_data %>%
  arrange(desc(calories)) %>%
  slice(1:3)

# Find the top 3 foods with the least calories
top_least_calories <- top_three_concentrated_data %>%
  arrange(calories) %>%
  slice(1:3)

# Find the top 3 foods closest to the mean calories
closest_to_mean_calories <- top_three_concentrated_data %>%
  arrange(diff_from_mean) %>%
  slice(1:3)

# Combine the subsets into one data frame for plotting
selected_items_from_top_3_data <- rbind(
  mutate(top_most_calories, Category = "Most Calories"),
  mutate(top_least_calories, Category = "Least Calories"),
  mutate(closest_to_mean_calories, Category = "Closest to Mean")
)

# Create the bar chart
ggplot(selected_items_from_top_3_data, aes(x = reorder(label, calories), y = calories, fill = Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Dish - Restaurant", y = "Calories", title = "Top Caloric Foods and Those Closest to Mean") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set1")

# look up the mean value for each concerned column
top_3_in_each_caloric_group <- data %>%
  summarise(
    mean_calories = mean(calories, na.rm = TRUE),
    mean_total_fat = mean(total_fat, na.rm = TRUE),
    mean_cholesterol = mean(cholesterol, na.rm = TRUE),
    mean_sodium = mean(sodium, na.rm = TRUE),
    mean_fiber = mean(fiber, na.rm = TRUE),
    mean_protein = mean(protein, na.rm = TRUE),
    mean_sugar = mean(sugar, na.rm = TRUE)
  )
print(top_3_in_each_caloric_group)

########################################################################################################
##1st Hypothesis - Various restaurants offer unique nutritional profiles, with some potentially offering healthier options compared to others.
########################################################################################################
# Calculate the average nutrition in each restaurant
average_nutrition <- data %>%
  group_by(restaurant) %>%
  summarise(
    Avg_Calories = mean(calories, na.rm = TRUE),
    Avg_TotalFat = mean(total_fat, na.rm = TRUE),
    Avg_TotalCarb = mean(total_carb, na.rm = TRUE),
    Avg_Protein = mean(protein, na.rm = TRUE)
  )
ggplot(average_nutrition, aes(x = reorder(restaurant, -Avg_Calories), y = Avg_Calories)) +
  geom_bar(stat = "identity", fill = 'steelblue') +
  labs(x = "Restaurant", y = "Average Calories", title = "Average Calories per Restaurant") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

########################################################################################################

# A male adult consumes 2000 cals per day in 3 meals.
total_intake_calories <- 2000
calories_per_meal <- 666
low_calories_threshold <- 333

# Function to perform a one-sample t-test for each restaurant
compared_avg_calories_t_test <- function(restaurant_data) {
  if (nrow(restaurant_data) > 1) {
    test_result <- t.test(restaurant_data$calories, mu = 666)
    return(test_result$p.value)
  } else {
    return(NA)  # Return NA if not enough observations
  }
}

########################################################################################################

##############2nd Hypothesis: Many items from these diverse restaurant menus could constitute a complete daily meal for an adult.
# Add a new column to categorize items based on their calorie content
data$Meal_Suitability <- ifelse(data$calories <= 666, "Suitable", "Not Suitable")

# Calculate the proportion of suitable items in each restaurant
suitability_proportion <- data %>%
  group_by(restaurant) %>%
  summarise(Suitable_Items = mean(Meal_Suitability == "Suitable"))
print(suitability_proportion)

# Perform a one-sample t-test for each restaurant against the 666 calorie benchmark
t_test_healthy_restaurant <- data %>%
  group_by(restaurant) %>%
  summarise(p_value = t.test(calories, mu = 666)$p.value)
# Display results
print(t_test_healthy_restaurant)

ggplot(suitability_proportion, aes(x = restaurant, fill = Suitable_Items)) +
  geom_bar(position = "fill") +
  labs(x = "Restaurant", y = "Proportion", title = "Proportion of dishes meets the standard meal Categories per Restaurant") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


########################################################################################################
# sodium
if ("sodium" %in% colnames(data)) {
  avg_sodium <- data %>%
    group_by(restaurant) %>%
    summarise(Avg_Sodium = mean(sodium, na.rm = TRUE))
  
  print(avg_sodium)
}

########################## the inflection of fat, protein and fiber on calories.
  # Scatter plots using pivot_longer
  data %>%
    pivot_longer(cols = c(total_fat, fiber, protein), names_to = "nutrient", values_to = "value") %>%
    ggplot(aes(x = value, y = calories)) +
    geom_point(aes(color = restaurant), alpha = 0.6) + 
    facet_wrap(~ nutrient, scales = "free") +
    labs(x = "Nutrient Value", y = "Calories", title = "Reflection of Nutrients on Calories") +
    theme_minimal()

  
  
  
  
  
  
  ########################################################################################################
  # score the restaurants and dishes to find the healthy items and healthy restaurants.
  ########################################################################################################
  # Normalize each nutritional component by its maximum value
  max_protein <- max(data$protein, na.rm = TRUE)
  max_fiber <- max(data$fiber, na.rm = TRUE)
  max_fat <- max(data$total_fat, na.rm = TRUE)
  max_calories <- max(data$calories, na.rm = TRUE)
  
  mutated_data_to_score <- data %>%
    mutate(
      normalized_protein = protein / max_protein,
      normalized_fiber = fiber / max_fiber,
      normalized_fat = total_fat / max_fat,
      normalized_calories = calories / max_calories,
      # Healthiness score calculation
      healthiness_score = normalized_protein + 2 * normalized_fiber - normalized_fat - 0.5 * normalized_calories,
      # Add restaurant name to item for labeling
      label = paste(restaurant, "-", item)
    )

  # Define health categories based on healthiness score thresholds
  health_categories <- mutated_data_to_score %>%
    mutate(
      health_category = case_when(
        healthiness_score > quantile(healthiness_score, 0.66, na.rm = TRUE) ~ "Healthy",
        healthiness_score < quantile(healthiness_score, 0.33, na.rm = TRUE) ~ "Unhealthy",
        TRUE ~ "So-so"
      )
    )
  #Finding the top 10
  top_10_healthiest_foods <- health_categories %>%
    arrange(desc(healthiness_score)) %>%
    slice(1:10)
  # Visualize healthiness score distribution
  ggplot(top_10_healthiest_foods, aes(x = reorder(label, healthiness_score), y = healthiness_score, fill = health_category)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Menu Item", y = "Healthiness Score", fill = "Category", title = "Top 10 Healthiest Food Items")
  
  # Summarize the number of healthy items per restaurant
  restaurant_healthiness <- health_categories %>%
    group_by(restaurant) %>%
    summarise(
      healthy_count = sum(health_category == "Healthy", na.rm = TRUE),
      so_so_count = sum(health_category == "So-so", na.rm = TRUE),
      unhealthy_count = sum(health_category == "Unhealthy", na.rm = TRUE)
    )
  # Visualize the number of healthy items per restaurant
  ggplot(restaurant_healthiness, aes(x = reorder(restaurant, healthy_count), y = healthy_count)) +
    geom_bar(stat = "identity", fill = "green") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Restaurant", y = "Count of Healthy Items", title = "Number of Healthy Items by Restaurant")

  
  
  
  
  
  ########################################################################################################
# Decision support tree
tree <- rpart(restaurant ~ total_fat + fiber + protein + calories, data = data, method = "class")
rpart.plot(tree, 
           main = "Decision Tree for Predicting Restaurant Category Based on Nutrient Information",
           box.palette = "Greens", 
           type = 4, 
           extra = 101)
