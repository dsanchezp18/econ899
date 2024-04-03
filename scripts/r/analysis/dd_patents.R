# R Script: Difference-in-Differences Models with Two-Way Fixed Effects (TWFE) for patents as DVs
# ECON899 MA Paper 
# SFU Economics
# Daniel Sanchez
# Spring 2024 

# This script implements difference-in-differences models with two-way fixed effects (TWFE) for patents as dependent variables.

# Preliminaries -----------------------------------------------------------

# Load packages

library(dplyr, warn.conflicts = FALSE)
library(fixest, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(kableExtra, warn.conflicts = FALSE)
library(modelsummary, warn.conflicts = FALSE)
library(forcats, warn.conflicts = FALSE)
library(tibble, warn.conflicts = FALSE)
library(sandwich, warn.conflicts = FALSE)

# Load full dataset

df_full <- readRDS("data/full_dataset.rds")

# Define treatment start date

treatment_start_date <- ymd("2016-08-01")

# Define valid start and end dates

start_date <- ymd("2002-08-01")

end_date <- ymd("2021-08-01")

# Define valid start and end periods

start_period <- interval(start_date, treatment_start_date)/months(1)

end_period <- interval(treatment_start_date, end_date)/months(1)

# Number of years between start and end dates

years_between_dates <- interval(start_date, end_date)/years(1)

# Number of years between start and treatment dates

years_between_dates_treatment <- interval(start_date, treatment_start_date)/years(1)

# Number of years between treatment and end dates

years_between_dates_end <- interval(treatment_start_date, end_date)/years(1)

# Define df (relevant data) within the valid start period and end period already defined

df <- 
    df_full %>%
    filter(month_year %>% between(start_date, end_date), 
           !(province_code %in% c("MB", "PE")))

# Create the DID dummy in a modified dataframe
# Using the interaction operator i from the fixest package

df_twfe <- 
    df %>% 
    mutate(treated = fixest::i(treatment,post, ref = "Control", ref2 = "Pre")) 

# All patents -----------------------------------------------------------

## Baseline -----------------------------------------------------------

# Estimate a model without controls 

baseline_twfe <-
    feols(fml = ln1patents_filed ~ treated,
          fixef = c("province_code", "month_year"), 
          data = df_twfe,
          cluster = ~ province_code + month_year
    )

summary(baseline_twfe)

## Defendable controls -----------------------------------------------------------

def_controls <- "+ ln_total_pop + ln_total_emp + ln_total_median_wage + cpi + ln_exports_all_countries + ln_imports_all_countries + ln_retail_sales + ln_wholesale_sales + ln_manufacturing_sales + exp_index_econ_activity"

def_controls_twfe <-
    feols(fml = paste("ln1patents_filed ~ treated", def_controls) %>% as.formula(),
          fixef = c("province_code", "month_year"), 
          data = df_twfe,
          cluster = ~ province_code + month_year
    )

summary(controls1_twfe)

## Additional controls -----------------------------------------------------------

extra_controls <- "+ ln_average_actual_hours + ln1business_bankruptcies + new_housing_price_index + ln_food_services_receipts + log(wages_paid_patenting_ind) + total_avg_tenure + foreign_parties"

add_controls <- paste(def_controls, extra_controls)

add_controls_twfe <-
    feols(fml = paste("ln1patents_filed ~ treated", add_controls) %>% as.formula(),
          fixef = c("province_code", "month_year"), 
          data = df_twfe,
          cluster = ~ province_code + month_year
    )

summary(add_controls_twfe)

## See results with modelsummary -----------------------------------------------------------

# Load modelsummary parameters

source("scripts/r/modelsummary/stars.R")

patent_dd_models <- list(baseline_twfe, def_controls_twfe, add_controls_twfe)

modelsummary(patent_dd_models, stars = stars)