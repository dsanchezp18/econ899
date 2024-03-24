# R Script: Difference-in-Differences Models with Two-Way Fixed Effects (TWFE)
# ECON899 MA Paper 
# SFU Economics
# Daniel Sanchez
# Spring 2024 

# This script implements the difference-in-differences models with two-way fixed effects (TWFE) for the paper's main results

# Preliminaries -----------------------------------------------------------

# Load packages

library(dplyr)
library(fixest)
library(lubridate)
library(modelsummary)
library(forcats)
library(tibble)
library(sandwich)

# Load data and create an employment variable for "industries which are likely to patent"

df <- 
    readRDS("data/full_dataset.rds") %>% 
    mutate(emp_patenting_ind = emp_manufacturing + emp_wholesale_and_retail + emp_media + emp_professional + emp_other + emp_healthcare,
           wages_paid_patenting_ind = wages_paid_manufacturing + wages_paid_wholesale_and_retail + wages_paid_media + wages_paid_professional + wages_paid_other + wages_paid_healthcare)

# Define parameters for modelsummary 

source("scripts/r/modelsummary/stars.R")
source("scripts/r/modelsummary/coef_mappings.R")

# Baseline models -----------------------------------------------------------

# Estimate baseline models without explanatory variables, all periods available for the data
# I will use both all parties and inventors only, and compare

## Two-way fixed effects (TWFE) -----------------------------------------------------------

# Estimate the same model but with two-way fixed effects.
# In TWFE, we need a variable which is the DID "dummy" (here called treated). Cannot perform the interaction in the model specification itself, as it would cause perfect collinearity.

# Create the DID dummy in a modified dataframe

df_twfe <- 
    df %>% 
    mutate(treated = fixest::i(treatment,post, ref = "Control", ref2 = "Pre")) # Using the interaction operator i from the fixest package

# Estimate the baseline TWFE model with all interested parties

baseline_twfe <-
    feols(ln_parties ~ treated | province_code + month_year, 
          data = df_twfe,
          cluster = ~ province_code + month_year)

summary(baseline_twfe)

# Repeat with inventors only

baseline_twfe_inventors <-
    feols(ln_inventors ~ treated | province_code + month_year, 
          data = df_twfe,
          cluster = ~ province_code + month_year)

summary(baseline_twfe_inventors)

# Repeat with applicants only

baseline_twfe_applicants <-
    feols(ln_applicants ~ treated | province_code + month_year, 
          data = df_twfe,
          cluster = ~ province_code + month_year)

summary(baseline_twfe_applicants)

# Repeat with owners only

baseline_twfe_owners <-
    feols(ln_owners ~ treated | province_code + month_year, 
          data = df_twfe,
          cluster = ~ province_code + month_year)

summary(baseline_twfe_owners)

# Repeat with patents as the dependent variable

baseline_twfe_patents <-
    feols(ln_patents_filed ~ treated | province_code + month_year, 
          data = df_twfe,
          cluster = ~ province_code + month_year)

summary(baseline_twfe_patents)

## Present baseline results with modelsummary -----------------------------------------------------------

# List of models

baseline_did_models <- list(baseline_twfe, baseline_twfe_inventors, baseline_twfe_applicants, baseline_twfe_owners)

modelsummary(baseline_did_models, stars = stars)

# Models with explanatory variables -----------------------------------------------------------

# Estimate the models with the explanatory variables included.

# Define a formula object with the summation of all explanatory variables to be included in the models 

explanatory_vars <- "~ log(total_pop) + log(total_emp) + log(total_median_wage) + cpi + log(business_insolvencies+1) + log(travellers) + new_housing_price_index + log(electric_power_generation + 1) + 
log(wages_paid_patenting_ind) + log(emp_patenting_ind) + log(exports_all_countries) + log(imports_all_countries)" %>% 
                    as.formula()

## Two-way fixed effects (TWFE) -----------------------------------------------------------

# All parties

model_explanatory_twfe <-
    feols(update(explanatory_vars, ln_parties ~ treated + .), 
          data = df_twfe,
          fixef =  c("province_code", "month_year"),
          cluster = ~ province_code + month_year)

summary(model_explanatory_twfe)

# Repeat with inventors only

model_explanatory_twfe_inventors <-
    feols(update(explanatory_vars, ln_inventors ~ treated + .), 
          data = df_twfe,
          fixef =  c("province_code", "month_year"),
          cluster = ~ province_code + month_year)
    
summary(model_explanatory_twfe_inventors)

# Repeat with applicants only

model_explanatory_twfe_applicants <-
    feols(update(explanatory_vars, ln_applicants ~ treated + .), 
          data = df_twfe,
          fixef =  c("province_code", "month_year"),
          cluster = ~ province_code + month_year)

summary(model_explanatory_twfe_applicants)

# Repeat with owners only

model_explanatory_twfe_owners <-
    feols(update(explanatory_vars, ln_owners ~ treated + .), 
          data = df_twfe,
          fixef =  c("province_code", "month_year"),
          cluster = ~ province_code + month_year)

summary(model_explanatory_twfe_owners)

# Repeat with patents as the dependent variable

model_explanatory_twfe_patents <-
    feols(update(explanatory_vars, ln_patents_filed ~ treated + ln_foreign_parties + .),
          data = df_twfe,
          fixef =  c("province_code", "month_year"),
          cluster = ~ province_code + month_year)
        
summary(model_explanatory_twfe_patents)

# Export main results ----------------------------------------------------------------

# Main results are the TWFE models with explanatory variables for all interested parties, inventors only, applicants only, and owners only, as well as the patent model with explanatory variables.

# List the models

main_did_models <- list(model_explanatory_twfe, model_explanatory_twfe_inventors, model_explanatory_twfe_applicants, model_explanatory_twfe_owners, model_explanatory_twfe_patents)

# Create a dataframe with the explained variable names to be added to the model

explained_vars <- tibble(
                term = "Explained variable (Ln)",
                v1 = "All parties",
                v2 = "Inventors",
                v3 = "Applicants",
                v4 = "Owners",
                v5 = "Patents filed"
)

# Change the position to the top of the table

attr(explained_vars, 'position') <- 0

# Coefficient names to be included in the table

## Display results -----------------------------------------------------------

modelsummary(main_did_models,
             stars = stars,
             add_rows = explained_vars,
             gof_omit = "AIC|BIC|RMSE",
             vcov = ~ province_code + month_year, # Cluster by province and month, shows nicely in the table
)

# Main models with ln+1 -----------------------------------------------------------

# Define the formula which I will update

formula_for_ln_1 <- "~ treated + log(total_pop) + log(total_emp) + log(total_median_wage) + cpi + log(business_insolvencies+1) + log(travellers) + new_housing_price_index + log(electric_power_generation + 1) + 
log(wages_paid_patenting_ind) + log(emp_patenting_ind) + log(exports_all_countries) + log(imports_all_countries)"  %>% 
                     as.formula()

# Run the models, for all parties, inventors, applicants, owners, and patents filed
# Update the formula with the new dependent variable which is Log of the variable + 1

model_explanatory_twfe_ln_1 <-
    feols(update(formula_for_ln_1, ln_parties_1 ~ .),
          data = df_twfe,
          fixef =  c("province_code", "month_year"),
          cluster = ~ province_code + month_year)

model_explanatory_twfe_inventors_ln_1 <-
    feols(update(formula_for_ln_1, ln_inventors_1 ~ .),
          data = df_twfe,
          fixef =  c("province_code", "month_year"),
          cluster = ~ province_code + month_year)

model_explanatory_twfe_applicants_ln_1 <-
    feols(update(formula_for_ln_1, ln_applicants_1 ~ .),
          data = df_twfe,
          fixef =  c("province_code", "month_year"),
          cluster = ~ province_code + month_year)
    
model_explanatory_twfe_owners_ln_1 <-
    feols(update(formula_for_ln_1, ln_owners_1 ~ .),
          data = df_twfe,
          fixef =  c("province_code", "month_year"),
          cluster = ~ province_code + month_year)

model_explanatory_twfe_patents_ln_1 <-
    feols(update(formula_for_ln_1, ln_patents_filed_1 ~ ln_foreign_parties + .),
          data = df_twfe,
          fixef =  c("province_code", "month_year"),
          cluster = ~ province_code + month_year)

# Present results with modelsummary

modelsummary(list(model_explanatory_twfe_ln_1, model_explanatory_twfe_inventors_ln_1, model_explanatory_twfe_applicants_ln_1, model_explanatory_twfe_owners_ln_1, model_explanatory_twfe_patents_ln_1), stars = stars)

# Bootstrap standard errors for main models -----------------------------------------------------------

# Use the sandwich package to bootstrap standard errors for the main models

B <- 1000

# All parties with booststrap

# Bootstrap

model_explanatory_twfe_boot <- 
    vcovBS(model_explanatory_ls, cluster = ~province_code + month_year, R = B)

model_explanatory_twfe <-
    feols(update(explanatory_vars, ln_parties ~ treated + .), 
          data = df_twfe,
          fixef =  c("province_code", "month_year"),
          vcov = model_explanatory_twfe_boot)

summary(model_explanatory_twfe)

# Present results with modelsummary

modelsummary(list(model_explanatory_twfe_ln_1, model_explanatory_twfe_inventors_ln_1, model_explanatory_twfe_applicants_ln_1, model_explanatory_twfe_owners_ln_1, model_explanatory_twfe_patents_ln_1), stars = stars)




