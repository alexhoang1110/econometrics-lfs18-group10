# Load required libraries
library(tidyverse)
library(stringr)
library(readr)

# Import raw dataset from CSV
df <- read_csv("data/df_raw.csv")

# Calculate 1% and 99% quantile bounds for wage (exclude non-positive values)
lower_bound <- quantile(df$C44A[df$C44A > 0], 0.01, na.rm = TRUE)
upper_bound <- quantile(df$C44A[df$C44A > 0], 0.99, na.rm = TRUE)

# Data cleaning pipeline and variable creation
df_clean <- df %>%

  # STEP 1: Filter missing values, apply wage bounds, and keep labor age >= 15
  filter(
    !is.na(C44A), C44A >= lower_bound, C44A <= upper_bound,
    !is.na(C17),
    !is.na(C29C),
    !is.na(C5), C5 >= 15,
    !is.na(C36),
    !is.na(C3),
    !is.na(TINH)
  ) %>%

  # STEP 2: Create new variables
  mutate(
    # Dependent variable: log of wage
    ln_wage = log(C44A),

    # Education dummy: 1 = university, 0 = others
    educ = ifelse(C17 %in% c(8, 9), 1, 0),

    # Service segment classification from occupation code
    level1 = substr(as.character(C29C), 1, 1),
    servseg = case_when(
      level1 %in% c("1", "2", "3") ~ 1, # High-end
      level1 %in% c("4", "5", "9") ~ 0, # Low-end
      TRUE ~ NA_real_
    ),

    # Interaction term: education × service segment
    educ_servseg = educ * servseg,

    # Years of schooling based on education code
    years_of_schooling = case_when(
      C17 == 1 ~ 0, C17 == 2 ~ 3, C17 == 3 ~ 5, C17 == 4 ~ 9,
      C17 == 5 ~ 12, C17 == 6 ~ 14, C17 == 7 ~ 15, C17 == 8 ~ 16, C17 == 9 ~ 18,
      TRUE ~ 0
    ),

    # Experience = Age - schooling years - 6, capped at 0
    exp = C5 - years_of_schooling - 6,
    exp = ifelse(exp < 0, 0, exp),
    exp_sq = exp^2,

    # Gender dummy: 1 = female, 0 = male
    fem = ifelse(C3 == 2, 1, 0),

    # Contract dummy: 1 = formal contract, 0 = informal/no contract
    cont = ifelse(C36 %in% c(1, 2, 3, 4, 5), 1, 0),

    # Region grouping into 6 socio-economic regions
    region_group = case_when(
      TINH %in% c(1, 17, 26, 27, 30, 31, 33, 34, 35, 36, 37) ~ "RRD",
      TINH %in% c(2, 4, 6, 8, 10, 11, 12, 14, 15, 19, 20, 22, 24, 25) ~ "NMM",
      TINH %in% c(38, 40, 42, 44, 45, 46, 48, 49, 51, 52, 54, 56, 58, 60) ~ "Coast",
      TINH %in% c(62, 64, 66, 67, 68) ~ "Highlands",
      TINH %in% c(70, 72, 74, 75, 77, 79) ~ "SE",
      TINH %in% c(80, 82, 83, 84, 86, 87, 89, 91, 92, 93, 94, 95, 96) ~ "MRD",
      TRUE ~ NA_character_
    ),
    reg_i = factor(region_group),
    reg_i = relevel(reg_i, ref = "RRD") # Red River Delta as baseline
  ) %>%

  # STEP 3: Keep only valid service segment observations
  filter(!is.na(servseg)) %>%

  # Select variables for regression analysis
  select(ln_wage, educ, servseg, educ_servseg, exp, exp_sq, fem, cont, reg_i, age = C5)

# Save cleaned dataset to CSV
write_csv(df_clean, "data/csv/df_clean.csv")

# Summary statistics of cleaned data
summary(df_clean)