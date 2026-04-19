# Required libraries
library(tidyverse)
library(stringr)
library(readr)

# Load the raw data
df <- read_csv("data/df_raw.csv")

# Determine the limitations outlier 1% and 99% for `wage`
lower_bound <- quantile(df$C44A[df$C44A > 0], 0.01, na.rm = TRUE)
upper_bound <- quantile(df$C44A[df$C44A > 0], 0.99, na.rm = TRUE)

# Pipeline cleaning data & create new variable
df_clean <- df %>%

# STEP 1: FILTERING & OUTLIERS
filter(
    !is.na(C44A), C44A >= lower_bound, C44A <= upper_bound,
    !is.na(C17),
    !is.na(C29C),
    !is.na(C5), C5 >= 15, # Labor age (>= 15)
    !is.na(C36),
    !is.na(C3),
    !is.na(TINH)
) %>%

# STEP 2: CREATE NEW VARIABLE
mutate(
    # 1. Dependent variable: ln_wage
    ln_wage = log(C44A),
    
    # 2. Education (educ): 1 = University (Code 8, 9), 0 = Others
    educ = ifelse(C17 %in% c(8, 9), 1, 0),

    # 3. Service segment (servseg)
    # Occupation level 1
    level1 = substr(as.character(C29C), 1, 1),
    servseg = case_when(
        level1 %in% c("1", "2", "3") ~ 1, # High-end
        level1 %in% c("4", "5", "9") ~ 0, # Low-end
        TRUE ~ NA_real_ # For any other cases, assign NA
    ),

    # 4. Interaction Term: educ_servseg
    educ_servseg = educ * servseg,

    # 5. Experience (exp) & exp_sq
    years_of_schooling = case_when(
        C17 == 1 ~ 0, C17 == 2 ~ 3, C17 == 3 ~ 5, C17 == 4 ~ 9,
        C17 == 5 ~ 12, C17 == 6 ~ 14, C17 == 7 ~ 15, C17 == 8 ~ 16, C17 == 9 ~ 18,
        TRUE ~ 0
    ),
    # Experience = Age - Years of schooling - 6
    exp = C5 - years_of_schooling - 6,
    exp = ifelse(exp < 0, 0, exp), # Set negative experience to 0
    exp_sq = exp^2,

    # 6. Gender (fem): C3 (1 = Male, 2 = Female)
    fem = ifelse(C3 == 2, 1, 0),

    # 7. Contract (cont): C36 (1 -> 5: have a formal contract, 6-7: oral/no contract)
    cont = ifelse(C36 %in% c(1, 2, 3, 4, 5), 1, 0),

    # 8. Region (reg_i): The 63 provinces and cities are divided into 6 socio-economic regions.
    region_group = case_when(
        TINH %in% c(1, 17, 26, 27, 30, 31, 33, 34, 35, 36, 37) ~ "RRD", # Red River Delta
        TINH %in% c(2, 4, 6, 8, 10, 11, 12, 14, 15, 19, 20, 22, 24, 25) ~ "NMM", # Northen Mountains
        TINH %in% c(38, 40, 42, 44, 45, 46, 48, 49, 51, 52, 54, 56, 58, 60) ~ "Coast", # Central Coast
        TINH %in% c(62, 64, 66, 67, 68) ~ "Highlands", # Highlands
        TINH %in% c(70, 72, 74, 75, 77, 79) ~ "SE", # Southeast
        TINH %in% c(80, 82, 83, 84, 86, 87, 89, 91, 92, 93, 94, 95, 96) ~ "MRD", # Mekong Delta
        TRUE ~ NA_character_
    ),
    # Variable factor with Red River Delta as Baseline
    reg_i = factor(region_group),
    reg_i = relevel(reg_i, ref = "RRD")
) %>%

# STEP 3: CLEANING
# Only keep the obs in service segment
filter(!is.na(servseg)) %>%

# Select only the relevant variables for regression
select(ln_wage, educ, servseg, educ_servseg, exp, exp_sq, fem, cont, reg_i, age = C5)

# Save the cleaned data
write_csv(df_clean, "data/csv/df_clean.csv")

summary(df_clean)