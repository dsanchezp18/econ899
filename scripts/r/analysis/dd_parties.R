# R Script: Difference-in-Differences Models with Two-Way Fixed Effects (TWFE) for parties as DVs
# ECON899 MA Paper 
# SFU Economics
# Daniel Sanchez
# Spring 2024 

# This script implements difference-in-differences models with two-way fixed effects (TWFE) for patent parties as dependent variables.

# Load full dataset

df_full <- readRDS("data/full_dataset.rds")

# Define treatment start date

treatment_start_date <- ymd("2016-08-01")

# Define valid start and end dates

start_date <- ymd("2001-08-01")

end_date <- ymd("2021-08-01")

df <- 
    df_full %>%
    filter(month_year %>% between(start_date, end_date))

# Create the DID dummy in a modified dataframe
# Using the interaction operator i from the fixest package

df_twfe <- 
    df %>% 
    mutate(treated = fixest::i(treatment,post, ref = "Control", ref2 = "Pre")) 

# Baseline DD -----------------------------------------------------------

baseline_twfe <-
    feols(fml = ln1pa ~ treated,
          fixef = c("province_code", "month_year"), 
          data = df_twfe,
          cluster = ~ province_code + month_year
    )

summary(baseline_twfe)
