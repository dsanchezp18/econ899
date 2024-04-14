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

df_full <- readRDS("data/full_data_quarterly.rds")

# Define treatment start date

treatment_start_date <- ymd("2016-04-01")

# Define valid start and end dates

start_date <- ymd("2001-08-01")

end_date <- ymd("2021-08-01")

# Get quarter dates with floor_date

start_date_quarter <- floor_date(start_date, "quarter")

end_date_quarter <- floor_date(end_date, "quarter")

treatment_start_date_quarter <- floor_date(treatment_start_date, "quarter")

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
    feols(fml = ln1_patents_filed ~ treated,
          fixef = c("province_code", "quarter_year"), 
          data = df_twfe,
          cluster = ~ province_code + quarter_year
    )

summary(baseline_twfe)

## Defendable controls -----------------------------------------------------------

def_controls <- "+ ln_total_pop + ln_total_full_emp + ln_total_median_wage + cpi + ln1_business_insolvencies + ln_exports_all_countries + ln_imports_all_countries + ln_retail_sales + ln_wholesale_sales + ln_manufacturing_sales + ln1_foreign_parties"

def_controls_twfe <-
    feols(fml = paste("ln1_patents_filed ~ treated", def_controls) %>% as.formula(),
          fixef = c("province_code", "quarter_year"), 
          data = df_twfe,
          cluster = ~ province_code + quarter_year
    )

summary(def_controls_twfe)

## Additional controls -------------------------------------------------------------------

extra_controls <- "+ travellers + vehicles + ln_electric_power_generation + ln_average_actual_hours + new_housing_price_index + ln_food_services_receipts + ln_total_avg_tenure"

add_controls <- paste(def_controls, extra_controls)

add_controls_twfe <-
    feols(fml = paste("ln1_patents_filed ~ treated", add_controls) %>% as.formula(),
          fixef = c("province_code", "quarter_year"), 
          data = df_twfe,
          cluster = ~ province_code + quarter_year
    )

summary(add_controls_twfe)

## See results with modelsummary -----------------------------------------------------------

# Load modelsummary parameters

source("scripts/r/modelsummary/stars.R")

patent_dd_models <- list(baseline_twfe, def_controls_twfe, add_controls_twfe)

modelsummary(patent_dd_models, stars = stars)

# Patent sections as DVs -----------------------------------------------------------

## Baseline -----------------------------------------------------------

baseline_twfe_A <-
    feols(fml = ln1_patents_A ~ treated,
          fixef = c("province_code", "quarter_year"), 
          data = df_twfe,
          cluster = ~ province_code + quarter_year
    )

baseline_twfe_B <-
    feols(fml = ln1_patents_B ~ treated,
          fixef = c("province_code", "quarter_year"), 
          data = df_twfe,
          cluster = ~ province_code + quarter_year
    )

baseline_twfe_C <-
    feols(fml = ln1_patents_C ~ treated,
          fixef = c("province_code", "quarter_year"), 
          data = df_twfe,
          cluster = ~ province_code + quarter_year
    )

baseline_twfe_D <-
    feols(fml = ln1_patents_D ~ treated,
          fixef = c("province_code", "quarter_year"), 
          data = df_twfe,
          cluster = ~ province_code + quarter_year
    )

baseline_twfe_E <-
    feols(fml = ln1_patents_E ~ treated,
          fixef = c("province_code", "quarter_year"), 
          data = df_twfe,
          cluster = ~ province_code + quarter_year
    )

baseline_twfe_F <-
    feols(fml = ln1_patents_F ~ treated,
          fixef = c("province_code", "quarter_year"), 
          data = df_twfe,
          cluster = ~ province_code + quarter_year
    )

baseline_twfe_G <-
    feols(fml = ln1_patents_G ~ treated,
          fixef = c("province_code", "quarter_year"), 
          data = df_twfe,
          cluster = ~ province_code + quarter_year
    )

baseline_twfe_H <-
    feols(fml = ln1_patents_H ~ treated,
          fixef = c("province_code", "quarter_year"), 
          data = df_twfe,
          cluster = ~ province_code + quarter_year
    )

baseline_twfe_Multiple <-
    feols(fml = ln1_patents_Multiple ~ treated,
          fixef = c("province_code", "quarter_year"), 
          data = df_twfe,
          cluster = ~ province_code + quarter_year
    )

# See results with modelsummary 
patent_dd_models_sections <- list(baseline_twfe_A, baseline_twfe_B, baseline_twfe_C, baseline_twfe_D, 
                                  baseline_twfe_E, baseline_twfe_F, baseline_twfe_G, baseline_twfe_H, 
                                  baseline_twfe_Multiple)
                    
modelsummary(patent_dd_models_sections, stars = stars)

## Defendable controls -----------------------------------------------------------

def_controls_twfe_A <-
    feols(fml = paste("ln1_patents_A ~ treated", def_controls) %>% as.formula(),
          fixef = c("province_code", "quarter_year"), 
          data = df_twfe,
          cluster = ~ province_code + quarter_year
    )

def_controls_twfe_B <-
    feols(fml = paste("ln1_patents_B ~ treated", def_controls) %>% as.formula(),
          fixef = c("province_code", "quarter_year"), 
          data = df_twfe,
          cluster = ~ province_code + quarter_year
    )

def_controls_twfe_C <-
    feols(fml = paste("ln1_patents_C ~ treated", def_controls) %>% as.formula(),
          fixef = c("province_code", "quarter_year"), 
          data = df_twfe,
          cluster = ~ province_code + quarter_year
    )

def_controls_twfe_D <-
    feols(fml = paste("ln1_patents_D ~ treated", def_controls) %>% as.formula(),
          fixef = c("province_code", "quarter_year"), 
          data = df_twfe,
          cluster = ~ province_code + quarter_year
    )

def_controls_twfe_E <-
    feols(fml = paste("ln1_patents_E ~ treated", def_controls) %>% as.formula(),
          fixef = c("province_code", "quarter_year"), 
          data = df_twfe,
          cluster = ~ province_code + quarter_year
    )

def_controls_twfe_F <-
    feols(fml = paste("ln1_patents_F ~ treated", def_controls) %>% as.formula(),
          fixef = c("province_code", "quarter_year"), 
          data = df_twfe,
          cluster = ~ province_code + quarter_year
    )

def_controls_twfe_G <-
    feols(fml = paste("ln1_patents_G ~ treated", def_controls) %>% as.formula(),
          fixef = c("province_code", "quarter_year"), 
          data = df_twfe,
          cluster = ~ province_code + quarter_year
    )

def_controls_twfe_H <-
    feols(fml = paste("ln1_patents_H ~ treated", def_controls) %>% as.formula(),
          fixef = c("province_code", "quarter_year"), 
          data = df_twfe,
          cluster = ~ province_code + quarter_year
    )

def_controls_twfe_Multiple <-
    feols(fml = paste("ln1_patents_Multiple ~ treated", def_controls) %>% as.formula(),
          fixef = c("province_code", "quarter_year"), 
          data = df_twfe,
          cluster = ~ province_code + quarter_year
    )

# See results with modelsummary
patent_dd_models_sections_def_controls <- list(def_controls_twfe_A, def_controls_twfe_B, def_controls_twfe_C, def_controls_twfe_D, 
                                              def_controls_twfe_E, def_controls_twfe_F, def_controls_twfe_G, def_controls_twfe_H, 
                                              def_controls_twfe_Multiple)
                                            
modelsummary(patent_dd_models_sections_def_controls, stars = stars)

## Additional controls -----------------------------------------------------------

add_controls_twfe_A <-
    feols(fml = paste("ln1_patents_A ~ treated", add_controls) %>% as.formula(),
          fixef = c("province_code", "quarter_year"), 
          data = df_twfe,
          cluster = ~ province_code + quarter_year
    )

add_controls_twfe_B <-
    feols(fml = paste("ln1_patents_B ~ treated", add_controls) %>% as.formula(),
          fixef = c("province_code", "quarter_year"), 
          data = df_twfe,
          cluster = ~ province_code + quarter_year
    )

add_controls_twfe_C <-
    feols(fml = paste("ln1_patents_C ~ treated", add_controls) %>% as.formula(),
          fixef = c("province_code", "quarter_year"), 
          data = df_twfe,
          cluster = ~ province_code + quarter_year
    )

add_controls_twfe_D <-
    feols(fml = paste("ln1_patents_D ~ treated", add_controls) %>% as.formula(),
          fixef = c("province_code", "quarter_year"), 
          data = df_twfe,
          cluster = ~ province_code + quarter_year
    )

add_controls_twfe_E <-
    feols(fml = paste("ln1_patents_E ~ treated", add_controls) %>% as.formula(),
          fixef = c("province_code", "quarter_year"), 
          data = df_twfe,
          cluster = ~ province_code + quarter_year
    )

add_controls_twfe_F <-
    feols(fml = paste("ln1_patents_F ~ treated", add_controls) %>% as.formula(),
          fixef = c("province_code", "quarter_year"), 
          data = df_twfe,
          cluster = ~ province_code + quarter_year
    )

add_controls_twfe_G <-
    feols(fml = paste("ln1_patents_G ~ treated", add_controls) %>% as.formula(),
          fixef = c("province_code", "quarter_year"), 
          data = df_twfe,
          cluster = ~ province_code + quarter_year
    )

add_controls_twfe_H <-
    feols(fml = paste("ln1_patents_H ~ treated", add_controls) %>% as.formula(),
          fixef = c("province_code", "quarter_year"), 
          data = df_twfe,
          cluster = ~ province_code + quarter_year
    )

add_controls_twfe_Multiple <-
    feols(fml = paste("ln1_patents_Multiple ~ treated", add_controls) %>% as.formula(),
          fixef = c("province_code", "quarter_year"), 
          data = df_twfe,
          cluster = ~ province_code + quarter_year
    )

# See results with modelsummary
patent_dd_models_sections_add_controls <- list(add_controls_twfe_A, add_controls_twfe_B, add_controls_twfe_C, add_controls_twfe_D, 
                                              add_controls_twfe_E, add_controls_twfe_F, add_controls_twfe_G, add_controls_twfe_H, 
                                              add_controls_twfe_Multiple)
                                             
modelsummary(patent_dd_models_sections_add_controls, stars = stars)