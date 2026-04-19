library(tidyverse)
library(readr)

df_clean <- read_csv("data/csv/df_clean.csv")

# Subsample 1: Young Cohort (Age 15 - 35)
df_young <- df_clean %>%
    filter(age >= 15 & age <= 35)

write_csv(df_young, "data/csv/df_young.csv")
nrow(df_young)

# Subsample 2: Middle-aged Cohort (Age above 35 - 55)
df_middle <- df_clean %>%
    filter(age > 35 & age <= 55)

write_csv(df_middle, "data/csv/df_middle.csv")
nrow(df_middle)