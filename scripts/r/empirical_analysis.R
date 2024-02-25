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
library(modelsummary)
library(fwildclusterboot)
library(forcats)

# Load data

df <- readRDS("data/full_dataset.rds")

# Define a treatment date (month-year the AITC was passed)

treatment_start_date <- ymd("2016-08-01")
treatment_group <- "AB"

# Define "relevant" periods, which are periods for which there is there is relevant data 

start_date <- ymd("1990-01-01")
end_date <- ymd("2022-12-31")

# Difference-in-Differences (DiD) -----------------------------------------------------------

# Implement difference-in-differences estimation models

## Least squares (LS/OLS) -----------------------------------------------------------

# Interact the treatment with the post period to estimate the DiD model. No fixed effects, just the interaction term and the treatment and period dummies.
# All available periods since 1869 (19th century) are used

model_ls <-
    lm(ln_parties ~ treatment * post, data = df)

summary(model_ls)

# Significant. Thank you Kanye, very cool!

# This model has no correction for heteroskedasticity or serial/cluster correlation.

# Repeat but only with relevant periods 

model_ls_relevant <-
    lm(ln_parties ~ treatment * post, data = df %>% filter(filing_month_year %>% between(start_date, end_date)))

summary(model_ls_relevant)

# No longer significant. 

## Two-way fixed effects models (TWFE) -----------------------------------------------------------

# In TWFE, we need a variable which is the DID "dummy" (here called treated), as performing the interaction in the model itself will cause the model to be perfectly collinear.
# This is because the inclusion of the interaction term as well as the fixed effect make us fall in a dummy variable trap. 

# Modified dataframe for the TWFE model

df_twfe <-
    df  %>% 
    mutate(treated = fixest::i(treatment,post, ref = "Control", ref2 = "Pre"))

# Estimate the TWFE model

model_twfe <- 
    feols(ln_parties ~ treated | province_code + filing_month_year, 
          data = df_twfe,
          cluster = ~ province_code + filing_month_year)

summary(model_twfe)

# Repeat but only with relevant periods

df_twfe_relevant <- 
    df_twfe %>% 
    filter(filing_month_year %>% between(start_date, end_date))

model_twfe_relevant <-
    feols(ln_parties ~ treated,
          fixef = c("province_code", "filing_month_year"),
          data = df_twfe_relevant,
          cluster = ~ province_code + filing_month_year)

summary(model_twfe_relevant)

# Present preliminary simple results with modelsummary

simple_models <-
    list("OLS" = model_ls,
         "OLS (Relevant Periods)" = model_ls_relevant,
         "TWFE" = model_twfe,
         "TWFE (Relevant Periods)" = model_twfe_relevant)

modelsummary(simple_models, stars = TRUE, gof_omit = "F-statistic")

# Wild Cluster Bootstrap -----------------------------------------------------------

# Implement wild cluster bootstrap for the TWFE model with relevant periods

# Use the boottest function from the fwildclusterboot package to implement the wild cluster bootstrap

set.seed(123)

dqrng::dqset.seed(123)

twfe_relevant_boot <- 
    boottest(model_twfe_relevant, 
             B = 9999, 
             clustid = c("filing_month_year", "province"),
             param = "treated")

# Present the results

summary(twfe_relevant_boot)

# Event study specifications -----------------------------------------------------------

# Implement event study specifications by interacting the treatment dummy (being alberta) with all period dummies except one period before the post period started. 
# Consider relevant periods only.

# First define a dataframe friendly to the coefplot function which I will use for a quick event study plot

df_twfe_relevant <- 
    df_twfe_relevant %>% 
    mutate(treatment_dummy = (treatment == "Treatment"))

# Estimate the event study specification

model_event_study <-
    feols(ln_parties ~ i(periods, treatment_dummy, ref = -1) | province_code + filing_month_year,
          data = df_twfe_relevant,
          cluster = ~ province_code + filing_month_year)

# Do the coefplot

png("figures/event_study_simple.png", 
    width = 30, 
    height = 15, 
    units = "cm",
    res = 800)

iplot(model_event_study)

dev.off()
