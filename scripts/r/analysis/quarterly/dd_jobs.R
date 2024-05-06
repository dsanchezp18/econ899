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
library(forcats, warn.conflicts = FALSE)
library(tibble, warn.conflicts = FALSE)
library(sandwich, warn.conflicts = FALSE)
library(stringr, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)

# Load full dataset

df_full <- readRDS("data/full_data_quarterly.rds")

# Define treatment start date

treatment_start_date <- ymd("2017-01-01")

# Define valid start and end dates

start_date <- ymd("2001-01-01")

end_date <- ymd("2021-06-01")

# Get quarter dates with floor_date

start_date_quarter <- floor_date(start_date, "quarter")

end_date_quarter <- floor_date(end_date, "quarter")

treatment_start_date_quarter <- floor_date(treatment_start_date, "quarter")

# Same but with the quarter formula

start_quarter_q <- quarter(start_date, type =  "year.quarter") %>% str_replace_all("\\.", "Q")

end_quarter_q <- quarter(end_date, type =  "year.quarter") %>% str_replace_all("\\.", "Q")

treatment_start_quarter_q <- quarter(treatment_start_date, type =  "year.quarter") %>% str_replace_all("\\.", "Q")

# Define valid start and end periods

start_period <- interval(start_date_quarter, treatment_start_date_quarter)/months(3)

end_period <- interval(treatment_start_date_quarter, end_date_quarter)/months(3)

# Define df (relevant data) within the valid start period and end period already defined

df <- 
    df_full %>%
    filter(quarter_year_date %>% between(start_date_quarter, end_date_quarter))

# Create the DID dummy in a modified dataframe
# Using the interaction operator i from the fixest package

df_twfe <- 
    df %>% 
    mutate(treated = fixest::i(treatment,post, ref = "Control", ref2 = "Pre")) 

# All patents -----------------------------------------------------------

## Baseline -----------------------------------------------------------

# Estimate a model without controls 

baseline_twfe <-
    feols(fml = ln_total_full~ treated,
          fixef = c("province_code", "quarter_year"), 
          data = df_twfe,
          cluster = ~ province_code + quarter_year
    )

## Defendable controls -----------------------------------------------------------

def_controls <- "+ ln_total_pop + ln_total_full_emp + ln_total_median_wage + cpi + ln1_business_insolvencies + ln_exports_all_countries + ln_imports_all_countries + ln_retail_sales + ln_wholesale_sales + ln_manufacturing_sales + ln1_foreign_parties"

def_controls_twfe <-
    feols(fml = paste("ln1_patents_filed ~ treated", def_controls) %>% as.formula(),
          fixef = c("province_code", "quarter_year"), 
          data = df_twfe,
          cluster = ~ province_code + quarter_year
    )

## Additional controls -------------------------------------------------------------------

extra_controls <- "+ ln1_travellers + ln1_vehicles + ln_electric_power_generation + ln_average_actual_hours + new_housing_price_index + ln_food_services_receipts + ln_total_avg_tenure"

add_controls <- paste(def_controls, extra_controls)

add_controls_twfe <-
    feols(fml = paste("ln1_patents_filed ~ treated", add_controls) %>% as.formula(),
          fixef = c("province_code", "quarter_year"), 
          data = df_twfe,
          cluster = ~ province_code + quarter_year
    )