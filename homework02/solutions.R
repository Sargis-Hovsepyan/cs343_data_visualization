# ========================================================
# Data Visualization Assignment 02
# 
# Name:   Sargis Hovsepyan
# Date:   16/02/2025
# Course: CS 343 Data Visualization
# ========================================================

# Load Required Libraries
library(ggplot2)
library(ggthemes)

library(dplyr)
library(readr)
library(scales)

library(stringr)

# Set Theme for Consistency
theme_set(theme_minimal())

######################################################
# Part 3:  Use the datasets provided to create graphs#
######################################################

# 4.  Create a Scatter Plot by generating 100 random values from both the normal and logistic 
#     distributions. The points should be brown and use theme_solarized with argument light 
#     set to false.

set.seed(123)

normal_values <- rnorm(100)
logistic_values <- rlogis(100)

data <- data.frame(normal = normal_values,logistic = logistic_values)

# Create the scatter plot with ggplot
ggplot(data, aes(x = normal, y = logistic)) +
  geom_point(color = "brown") +
  theme_solarized(light = FALSE)


#########################################
# Part 4:  Recreate the following graphs#
#########################################

lc_df <- read.csv('./data/lung_cancer_prediction_dataset.csv')
ap_df <- read.csv('./data/global_air_pollution_dataset.csv')

# 2. Use the gpplot2 package for this graph.

selected_countries <- c("Egypt", "Ethiopia", "France", "Germany", "Finland",
                        "Indonesia", "Italy", "Japan", "Mexico", "Myanmar",
                        "Nigeria", "Pakistan", "Philippines", "South Africa",
                        "Thailand", "Turkey", "China")

ap_agg <- ap_df %>%
  filter(Country %in% selected_countries) %>%
  group_by(Country) %>%
  summarise(Avg_PM2.5 = mean(PM2.5_AQI_Value, na.rm = TRUE))

lc_agg <- lc_df %>%
  filter(Country %in% selected_countries) %>%
  group_by(Country) %>%
  summarise(Annual_Deaths = sum(Annual_Lung_Cancer_Deaths, na.rm = TRUE))

df_merged <- left_join(ap_agg, lc_agg, by = "Country")

ggplot(df_merged, aes(x = Avg_PM2.5, y = Annual_Deaths, label = Country)) +
  geom_point(aes(color = Country, size = Annual_Deaths), alpha = 0.7) +
  geom_text(vjust = -1, check_overlap = TRUE) +
  labs(
    title = "PM2.5 AQI vs. Annual Lung Cancer Deaths",
    x = "PM2.5 AQI Value",
    y = "Annual Lung Cancer Deaths",
    color = "Country",
    size = "Annual_Lung_Cancer_Deaths"
  ) +
  guides(size = "none") +  # Hide size legend values while keeping the title
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", color = "darkred", size = 16, hjust = 0.5),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12),
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 10),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.grid.major = element_line(color = "gray", linetype = "dashed"),
    panel.grid.minor = element_line(color = "lightgray", linetype = "dashed")
  )

# 3. Use the ggplot2 package for this graph.

# TODO: Continue
