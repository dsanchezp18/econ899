# R Script: Difference-in-Differences Models with Two-Way Fixed Effects (TWFE) for parties as DVs
# ECON899 MA Paper 
# SFU Economics
# Daniel Sanchez
# Spring 2024 

# This script implements difference-in-differences models with two-way fixed effects (TWFE) for patent parties as dependent variables.

# Load full dataset

df_full <- readRDS("data/full_dataset_monthly.rds")

# Define treatment start date

treatment_start_date <- ymd("2016-04-01")

# Define valid start and end dates

start_date <- ymd("2001-01-01")

end_date <- ymd("2021-06-01")

df <- 
    df_full %>%
    filter(month_year %>% between(start_date, end_date))

# Create the DID dummy in a modified dataframe
# Using the interaction operator i from the fixest package

df_twfe <- 
    df %>% 
    mutate(treated = fixest::i(treatment,post, ref = "Control", ref2 = "Pre")) 

# Baseline DD -----------------------------------------------------------

baseline_twfe_all_parties <-
    feols(fml = ln1_interested_parties ~ treated,
          fixef = c("province_code", "month_year"), 
          data = df_twfe,
          cluster = ~ province_code + month_year
    )

baseline_twfe_inventors <-
    feols(fml = ln1_inventors ~ treated,
          fixef = c("province_code", "month_year"), 
          data = df_twfe,
          cluster = ~ province_code + month_year
    )

baseline_twfe_applicants <-
    feols(fml = ln1_applicants ~ treated,
          fixef = c("province_code", "month_year"), 
          data = df_twfe,
          cluster = ~ province_code + month_year
    )

baseline_twfe_owners <-
    feols(fml = ln1_owners ~ treated,
          fixef = c("province_code", "month_year"), 
          data = df_twfe,
          cluster = ~ province_code + month_year
    )

# See results with modelsummary

source("scripts/r/modelsummary/stars.R")

baseline_parties <- list(baseline_twfe_all_parties, baseline_twfe_inventors, baseline_twfe_applicants, baseline_twfe_owners)

modelsummary(baseline_parties, stars = stars)

# Defendable controls -----------------------------------------------------------

def_controls <- "+ ln_total_pop + ln_total_full_emp + ln_total_median_wage + cpi + ln_exports_all_countries + ln_imports_all_countries + ln_retail_sales + ln_wholesale_sales + ln_manufacturing_sales + log(foreign_parties+1)"

def_controls_twfe_all_parties <-
    feols(fml = paste("ln1_interested_parties ~ treated", def_controls) %>% as.formula(),
          fixef = c("province_code", "month_year"), 
          data = df_twfe,
          cluster = ~ province_code + month_year
    )

def_controls_twfe_inventors <-
    feols(fml = paste("ln1_inventors ~ treated", def_controls) %>% as.formula(),
          fixef = c("province_code", "month_year"), 
          data = df_twfe,
          cluster = ~ province_code + month_year
    )

def_controls_twfe_applicants <-
    feols(fml = paste("ln1_applicants ~ treated", def_controls) %>% as.formula(),
          fixef = c("province_code", "month_year"), 
          data = df_twfe,
          cluster = ~ province_code + month_year
    )

def_controls_twfe_owners <-
    feols(fml = paste("ln1_owners ~ treated", def_controls) %>% as.formula(),
          fixef = c("province_code", "month_year"), 
          data = df_twfe,
          cluster = ~ province_code + month_year
    )

# See results with modelsummary

def_controls_parties <- list(def_controls_twfe_all_parties, def_controls_twfe_inventors, def_controls_twfe_applicants, def_controls_twfe_owners)

modelsummary(def_controls_parties, stars = stars)

# Additional controls -------------------------------------------------------------------

extra_controls <- "+ ln1_business_insolvencies + ln_electric_power_generation + ln_average_actual_hours + new_housing_price_index + ln_food_services_receipts + ln_total_avg_tenure"

add_controls <- paste(def_controls, extra_controls)

add_controls_twfe_all_parties <-
    feols(fml = paste("ln1_interested_parties ~ treated", add_controls) %>% as.formula(),
          fixef = c("province_code", "month_year"), 
          data = df_twfe,
          cluster = ~ province_code + month_year
    )

add_controls_twfe_inventors <-
    feols(fml = paste("ln1_inventors ~ treated", add_controls) %>% as.formula(),
          fixef = c("province_code", "month_year"), 
          data = df_twfe,
          cluster = ~ province_code + month_year
    )

add_controls_twfe_applicants <-
    feols(fml = paste("ln1_applicants ~ treated", add_controls) %>% as.formula(),
          fixef = c("province_code", "month_year"), 
          data = df_twfe,
          cluster = ~ province_code + month_year
    )

add_controls_twfe_owners <-
    feols(fml = paste("ln1_owners ~ treated", add_controls) %>% as.formula(),
          fixef = c("province_code", "month_year"), 
          data = df_twfe,
          cluster = ~ province_code + month_year
    )

# See results with modelsummary

add_controls_parties <- list(add_controls_twfe_all_parties, add_controls_twfe_inventors, add_controls_twfe_applicants, add_controls_twfe_owners)

modelsummary(add_controls_parties, stars = stars)