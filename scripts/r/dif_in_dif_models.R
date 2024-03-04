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

# Present preliminary simple results with modelsummary -----------------------------------------------------------

# List of models

baseline_did_models <- list(baseline_ls, baseline_ls_inventors, baseline_twfe, baseline_twfe_inventors)

modelsummary(baseline_did_models, stars = stars)