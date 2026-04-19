# Required libraries
library(haven)
library(tidyverse)
library(readr)

df_raw <- read_dta("data/dta/LFS_2018 (3)_cut_dup.dta")

df_selected <- df_raw %>%
    select(TINH, C3, C5, C17, C29C, C36, C44A)

head(df_selected)

write_csv(df_selected, "data/csv/df_raw.csv")

rm(df_raw)
gc()