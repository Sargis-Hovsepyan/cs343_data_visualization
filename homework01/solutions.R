# ========================================================
# Data Visualization Assignment 01
# 
# Name:   Sargis Hovsepyan
# Date:   01/02/2025
# Course: CS 343 Data Visualization
# ========================================================

# Load Required Libraries
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)

# Set Theme for Consistency
theme_set(theme_minimal())


# Part 1: Data Cleaning and Exploration

df <- read_csv("./data/crime_data.csv")
head(df, n=5)

# Cleaning data from  > 50% missing value columns
threshold <- nrow(df) * 0.5
df_cleaned <- df %>% select(where(~ sum(is.na(.)) <= threshold))

# Correct the Date Format
df_cleaned <- df_cleaned %>%
  mutate(
    `DATE OCC` = as.Date(substr(`DATE OCC`, 1, 10), format = "%m/%d/%Y"),
    Year = year(`DATE OCC`),
    Month = month(`DATE OCC`),
    Day = day(`DATE OCC`),
    
    Hour = as.numeric(substr(`TIME OCC`, 1, 2))
  )

# Filter the Data
df_filtered <- df_cleaned %>%
  filter(
    year(`DATE OCC`) == 2023,
    `Crm Cd Desc` == "BURGLARY"
  )

# Group and Sort
df_sorted <- df_filtered %>%
  group_by(`AREA NAME`) %>%
  summarise(
    total_crimes = n(),
    avg_victim_age = mean(`Vict Age`, na.rm = TRUE)
  ) %>%
  arrange(desc(total_crimes))


# Part 3: Further Exploration (R only)

monthly_crimes <- df_cleaned %>%
  group_by(Month) %>%  # Group by Month
  summarise(crime_count = n())

weapon_crimes <- df %>%
  filter(!is.na(`Weapon Used Cd`)) %>%  # Filter rows where Weapon Used Cd is not NA
  summarise(weapon_crimes = n())

crimes_by_premis <- df %>%
  group_by(`Premis Desc`) %>%
  summarise(crime_count = n())


# Part 4: Advanced Analysis

df_with_severity <- df %>%
  mutate(
    `Severity Score` = case_when(
      !is.na(`Weapon Used Cd`) ~ 5,
      `Crm Cd Desc` == "BURGLARY" ~ 3,
      TRUE ~ 1
    )
  )

severity_by_area <- df_with_severity %>%
  group_by(`AREA NAME`) %>%
  summarise(total_score = sum(`Severity Score`))