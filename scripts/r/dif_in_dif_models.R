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

## Two-way fixed effects (TWFE) -----------------------------------------------------------

# Estimate the same model but with two-way fixed effects.
# In TWFE, we need a variable which is the DID "dummy" (here called treated). Cannot perform the interaction in the model specification itself, as it would cause perfect collinearity.

# Create the DID dummy in a modified dataframe

df_twfe <- 
    df  %>% 
    mutate(treated = fixest::i(treatment,post, ref = "Control", ref2 = "Pre")) # Using the interaction operator i from the fixest package

# Estimate the baseline TWFE model with all interested parties

baseline_twfe <-
    feols(ln_parties ~ treated | province_code + filing_month_year, 
          data = df_twfe,
          cluster = ~ province_code + filing_month_year)

summary(baseline_twfe)

# Repeat with inventors only

baseline_twfe_inventors <-
    feols(ln_inventors ~ treated | province_code + filing_month_year, 
          data = df_twfe,
          cluster = ~ province_code + filing_month_year)

summary(baseline_twfe_inventors)

# Present baseline results with modelsummary -----------------------------------------------------------

# List of models

baseline_did_models <- list(baseline_ls, baseline_ls_inventors, baseline_twfe, baseline_twfe_inventors)

modelsummary(baseline_did_models, stars = stars)

# Models with explanatory variables -----------------------------------------------------------

# Estimate the models with the explanatory variables included.

# Define a formula object with the summation of all explanatory variables to be included in the models

explanatory_vars <- "~ log(total_pop) + log(total_emp) + log(total_median_wage) + log(total_average_hours) + log(ei_claims) + cpi + log(retail_sales) + log(wholesale_sales) + log(manufacturing_sales) + log(international_merchandise_imports) + new_housing_price_index"  %>% 
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

## Two-way fixed effects (TWFE) -----------------------------------------------------------

# Estimate the same model but with two-way fixed effects (using the same explanatory variables and DID dummy).

# All parties

model_explanatory_twfe <-
    feols(update(explanatory_vars, ln_parties ~ treated + .), 
          data = df_twfe,
          cluster = ~ province_code + filing_month_year)

summary(model_twfe)

# Repeat with inventors only

model_explanatory_twfe_inventors <-
    feols(update(explanatory_vars, ln_inventors ~ treated + .), 
          data = df_twfe,
          cluster = ~ province_code + filing_month_year)

summary(model_twfe_inventors)

# Export results ----------------------------------------------------------------

# List of models (only TWFE with explanatory)
did_models_explanatory <- list(model_explanatory_twfe, 
                               model_explanatory_twfe_inventors)

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
