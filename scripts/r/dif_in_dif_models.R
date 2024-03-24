# R Script: Difference-in-Differences Models
# ECON899 MA Paper 
# SFU Economics
# Daniel Sanchez
# Spring 2024 

# This script implements the difference-in-differences models. 

# Preliminaries -----------------------------------------------------------

# Load packages

library(dplyr)
library(fixest)
library(lubridate)
library(modelsummary)
library(forcats)
library(tibble)

# Load data

df <- readRDS("data/full_dataset.rds")

# Define parameters for modelsummary 

stars <- c('*'= 0.1, '**' = 0.05, '***'= 0.01)

# Baseline models -----------------------------------------------------------

# Estimate baseline models without explanatory variables, all periods available for the data
# I will use both all parties and inventors only, and compare

## Least squares (LS/OLS) -----------------------------------------------------------

# Estimate the simplest kind of model without any fixed effects, just the treatment and period dummies. All parties

baseline_ls <-
    lm(ln_parties ~ treatment * post, data = df)

summary(baseline_ls)

# Repeat with inventors only

baseline_ls_inventors <-
    lm(ln_inventors ~ treatment * post, data = df)

summary(baseline_ls_inventors)

# Repeat with applicants only

baseline_ls_applicants <-
    lm(ln_applicants ~ treatment * post, data = df)

summary(baseline_ls_applicants)

# Repeat with owners only

baseline_ls_owners <-
    lm(ln_owners ~ treatment * post, data = df)

summary(baseline_ls_owners)

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

## Present baseline results with modelsummary -----------------------------------------------------------

# List of models

baseline_did_models <- list(baseline_ls, baseline_ls_inventors, baseline_ls_applicants, baseline_ls_owners, baseline_twfe, baseline_twfe_inventors, baseline_twfe_applicants, baseline_twfe_owners)

modelsummary(baseline_did_models, stars = stars)

# Models with explanatory variables -----------------------------------------------------------

# Estimate the models with the explanatory variables included.

# Define a formula object with the summation of all explanatory variables to be included in the models 

explanatory_vars <- "~ log(total_pop) + log(total_emp) + log(total_median_wage) + log(average_actual_hours) + log(ei_claims) + cpi + log(retail_sales) + log(wholesale_sales) + log(manufacturing_sales) + log(international_merchandise_imports) + new_housing_price_index + log(food_services_receipts)+ log(travellers) + log(building_permits) + log(total_insolvencies)"  %>% 
                    as.formula()

## Least squares (LS/OLS) -----------------------------------------------------------

# Estimate the model with explanatory variables, all parties

model_explanatory_ls <-
    model_ls <- lm(update(explanatory_vars, ln_parties ~ treatment * post + .), data = df)

summary(model_ls)

# Repeat with inventors only

model_explanatory_ls_inventors <-
    model_ls_inventors <- lm(update(explanatory_vars, ln_inventors ~ treatment * post + .), data = df)

summary(model_ls_inventors)

# Repeat with applicants only

model_explanatory_ls_applicants <-
    model_ls_applicants <- lm(update(explanatory_vars, ln_applicants ~ treatment * post + .), data = df)

# Repeat with owners only

model_explanatory_ls_owners <-
    model_ls_owners <- lm(update(explanatory_vars, ln_owners ~ treatment * post + .), data = df)

## Two-way fixed effects (TWFE) -----------------------------------------------------------

# Estimate the same model but with two-way fixed effects (using the same explanatory variables and DID dummy).

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

# Present explanatory model results with modelsummary -----------------------------------------------------------

# List of models with explanatory variables

did_models_explanatory <- list(model_explanatory_ls, model_explanatory_ls_inventors, model_explanatory_ls_applicants, model_explanatory_ls_owners,
                               model_explanatory_twfe, model_explanatory_twfe_inventors, model_explanatory_twfe_applicants, model_explanatory_twfe_owners)

# Present with modelsummary 

modelsummary(did_models_explanatory, stars = stars)

# Models with patents as dependent variable -----------------------------------------------------------

## Baseline models without explanatory variables -----------------------------------------------------------

# Estimate baseline models without explanatory variables, all periods available for the data

# Estimate the baseline LS model with patents filed as dependent variable

baseline_ls_patents <-
    lm(ln_patents_filed ~ treatment * post, data = df)

summary(baseline_ls_patents)

# Estimate the baseline TWFE model with patents filed as dependent variable

model_baseline_twfe_patents <-
    feols(ln_patents_filed ~ treated | province_code + month_year, 
          data = df_twfe,
          cluster = ~ province_code + month_year)

summary(model_baseline_twfe_patents)

## Models with explanatory variables -----------------------------------------------------------

# Use the same formula, just updating it with the new dependent variable and a new control which is number of foreign interested parties

model_explanatory_twfe_patents <-
    feols(update(explanatory_vars, ln_patents_filed ~ treated + ln_foreign_parties + .),
          data = df_twfe,
          fixef =  c("province_code", "month_year"),
          cluster = ~ province_code + month_year)
        
summary(model_baseline_twfe_patents)

## Present results with modelsummary -----------------------------------------------------------

# Present results of the patent models with modelsummary

modelsummary(list(baseline_ls_patents, model_baseline_twfe_patents, model_explanatory_twfe_patents), stars = stars)

# Export results ----------------------------------------------------------------

# List of models (only TWFE with explanatory)

did_models_explanatory <- list(model_explanatory_twfe, model_explanatory_twfe_inventors, model_explanatory_twfe_applicants, model_explanatory_twfe_owners)

# Quick modelsummary with stars

modelsummary(did_models_explanatory, stars = stars)

# Create a dataframe with the extra columns for the models

rows <- tibble(
    term = "Explained variable",
    v1 = "ln(All interested parties)",
    v2 = "ln(Inventors only)"
)

# Change the position to the top of the table

attr(rows, 'position') <- 0

# Coefficient names to be included in the table

coef_names <- 
    c("(Intercept)" = "Intercept",
      "treated" = "Treatment x Post", 
      "log(total_pop)" = "Log(Total Population)", 
      "log(total_emp)" = "Log(Total Employment)", 
      "log(total_median_wage)" = "Log(Median Employee Wage)", 
      "log(total_average_hours)" = "Log(Average Hours)", 
      "log(ei_claims)" = "Log(EI Claims)", 
      "cpi" = "CPI Level", 
      "log(retail_sales)" = "Log(Retail Sales)", 
      "log(wholesale_sales)" = "Log(Wholesale Sales)", 
      "log(manufacturing_sales)" = "Log(Manufacturing Sales)", 
      "log(international_merchandise_imports)" = "Log(International Merchandise Imports)", 
      "new_housing_price_index" = "New Housing Price Index")

## Export results to a Word document -----------------------------------------------------------

modelsummary(did_models_explanatory, 
             stars = stars,
             add_rows = rows,
             coef_map = coef_names,
             gof_omit = "AIC|BIC|RMSE",
             output = "output/results/did_models_twfe.docx")
