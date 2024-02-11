# R Script: Empirical Analysis
# ECON899 MA Paper 
# SFU Economics
# Daniel Sanchez
# Spring 2024 

# This script implements the performs the empirical analysis for the paper's main results

# Preliminaries -----------------------------------------------------------

# Load packages

library(dplyr)
library(fixest)
library(lubridate)

# Load data

df <- readRDS("data/full_dataset.rds")

# Define a treatment date (month-year the AITC was passed)

treatment_date <- as.Date("2016-08-01")
treatment_group <- "AB"

# Difference-in-Differences (DiD) -----------------------------------------------------------

# Implement difference-in-differences estimation models

## Least squares (LS/OLS) -----------------------------------------------------------

# Interact the treatment with the post period to estimate the DiD model. No fixed effects, just the interaction term and the treatment and period dummies.

model_ls <-
    lm(ln_parties ~ treatment * post, data = df)

summary(model_ls)

# This model has no correction for heteroskedasticity or serial/cluster correlation.

## Two-way fixed effects models (TWFE) -----------------------------------------------------------

# In TWFE, we need a variable which is the DID "dummy" (here called treated), as performing the interaction in the model itself will cause the model to be perfectly collinear.
# This is because the inclusion of the interaction term as well as the fixed effect make us fall in a dummy variable trap. 

df_twfe <- 
    df %>% 
    mutate(treatment_period_int = interaction(treatment, post),
           treated = if_else(treatment_period_int == "Treatment.Post",1,0))

# Estimate the TWFE model

model_twfe <- 
    feols(ln_parties ~ treated | province + filing_month_year, 
          data = df_twfe,
          cluster = ~ province + filing_month_year)

summary(model_twfe)