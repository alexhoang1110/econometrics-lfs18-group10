# LOAD NECESSARY LIBRARIES AND DATASETS

# Required libraries for regression and diagnostics
library(readr)
library(tidyverse)
library(car)        # Variance Inflation Factor (VIF) test
library(lmtest)     # Breusch-Pagan test for heteroskedasticity
library(sandwich)   # Robust standard errors
library(stargazer)  # Regression output tables

# Load datasets (full sample, young cohort, middle-aged cohort)
df_full <- read_csv("data/df_clean.csv")
df_young <- read_csv("data/df_young.csv")
df_middle <- read_csv("data/df_middle.csv")

# Convert region variable to factor type
df_full$reg_i <- as.factor(df_full$reg_i)
df_young$reg_i <- as.factor(df_young$reg_i)
df_middle$reg_i <- as.factor(df_middle$reg_i) 

# REGRESSION MODELS
# Define regression formula (extended Mincerian wage equation)
formula_model <- ln_wage ~ educ + servseg + educ_servseg + exp + exp_sq + fem + cont + reg_i

# Estimate models for full sample, young cohort, and middle-aged cohort
model_full <- lm(formula_model, data = df_full)
model_young <- lm(formula_model, data = df_young)
model_middle <- lm(formula_model, data = df_middle)

# DIAGNOSTIC TESTS

# 1. VIF Test (check multicollinearity)
print("VIF Test for Full Sample:")
vif_results <- vif(model_full)
print(vif_results)

# 2. Breusch-Pagan Test (check heteroskedasticity)
print("Breusch-Pagan Test for Full Sample:")
# H0: Homoscedasticity. Reject H0 if p-value < 0.05.
bp_test <- bptest(model_full)
print(bp_test)

# EXPORT RESULTS WITH ROBUST STANDARD ERRORS

# Compute robust standard errors for each model
robust_se_full <- sqrt(diag(vcovHC(model_full, type = "HC1")))
robust_se_young <- sqrt(diag(vcovHC(model_young, type = "HC1")))
robust_se_middle <- sqrt(diag(vcovHC(model_middle, type = "HC1")))

# Generate regression tables with stargazer (HTML output)
stargazer(model_full, model_young, model_middle,
          type = "html",
          out = "docs/Regression_Results.doc",
          title = "Table 1: Estimation Results of Extended Mincerian Wage Equation",
          column.labels = c("Full Sample", "Young Cohort (15-35)", "Middle-aged Cohort (36-55)"),
          
          # Insert robust standard errors
          se = list(robust_se_full, robust_se_young, robust_se_middle),
          
          dep.var.labels = "Log(Wage)",
          covariate.labels = c("Education (Degree+)", "Service Segment (High-end)",
                               "Edu x Service", "Experience", "Experience Squared",
                               "Female", "Formal Contract"),

          omit = "reg_i", 
          add.lines = list(c("Region Controls", "Yes", "Yes", "Yes")),

          star.cutoffs = c(0.1, 0.05, 0.01),
          notes = "Note: Robust standard errors are reported in parentheses.",
          notes.align = "l")