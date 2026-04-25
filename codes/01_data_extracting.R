# Load required libraries
library(haven)
library(tidyverse)
library(readr)

# Import raw data from Stata file (.dta)
df_raw <- read_dta("data/dta/LFS_2018 (3)_cut_dup.dta")

# Select relevant variables from the dataset
df_selected <- df_raw %>%
    select(TINH, C3, C5, C17, C29C, C36, C44A)

# Preview the first rows of the selected data
head(df_selected)

# Export the selected data to CSV format
write_csv(df_selected, "data/csv/df_raw.csv")

# Free memory by removing the raw dataset
rm(df_raw)
gc()    # Run garbage collection to release memory