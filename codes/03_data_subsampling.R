# Load required libraries
library(tidyverse)
library(readr)

# Import cleaned dataset
df_clean <- read_csv("data/csv/df_clean.csv")

# Subsample 1: Young cohort (ages 15–35)
df_young <- df_clean %>%
    filter(age >= 15 & age <= 35)

# Save young cohort data and check sample size
write_csv(df_young, "data/csv/df_young.csv")
nrow(df_young)

# Subsample 2: Middle-aged cohort (ages 36–55)
df_middle <- df_clean %>%
    filter(age > 35 & age <= 55)

# Save middle-aged cohort data and check sample size
write_csv(df_middle, "data/csv/df_middle.csv")
nrow(df_middle)