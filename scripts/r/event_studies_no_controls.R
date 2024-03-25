# R Script: Event Study Regressions
# ECON899 MA Paper 
# SFU Economics
# Daniel Sanchez
# Spring 2024 

# This script implements event study regressions.

# Preliminaries -----------------------------------------------------------

library(dplyr)
library(fixest)
library(lubridate)
library(modelsummary)
library(forcats)
library(gridExtra)
library(ggplot2)
library(patchwork)

# Load data

df <- readRDS("data/full_dataset.rds")

# Define a treatment date (month-year the AITC was passed)

treatment_start_date <- ymd("2016-08-01")
treatment_group <- "AB"

# Define "relevant" periods, which are periods for which there is there is relevant data 

start_date <- ymd("1990-01-01")
end_date <- ymd("2022-12-31")

# Baseline event study specifications -----------------------------------------------------------

# Implement event study specifications by interacting the treatment dummy (being Alberta) with all period dummies except one period before the post period started. 
# Baseline models will be estimated with fixed effects only, no other controls. 
# Estimate with all periods available for the data, and with all parties and only inventors as well

# Create a dataframe which has the treatment dummy, equal to 1 whenever the treatment group is Alberta, and the period dummies.

df_event_study <- 
    df %>% 
    filter(periods %>% between(-487, 70)) %>% 
    mutate(period_dummies = as_factor(periods) %>% fct_relevel("-1"),
           event_study_dummies = fixest::i(period_dummies, treatment, ref = "-1", ref2 = "Control"),
           treatment_dummy = (treatment == "Treatment"))

# Do it again, but with a iplot-friendly specification

baseline_event_study_iplot <-
    feols(ln_parties ~ i(periods, treatment_dummy, ref = -1) | province_code + periods,
          data = df_event_study,
          cluster = ~ province_code + periods)

summary(baseline_event_study_iplot)

iplot(baseline_event_study_iplot)

# Save the chart

png("figures/event_study_baseline.png", 
    width = 17, 
    height = 10, 
    units = "cm",
    res = 800)

iplot(baseline_event_study_iplot)

dev.off()

# Repeat with inventors only

baseline_event_study_inventors <-
    feols(ln_inventors ~ i(periods, treatment_dummy, ref = -1) | province_code + periods,
          data = df_event_study,
          cluster = ~ province_code + periods)

summary(baseline_event_study_inventors)

iplot(baseline_event_study_inventors)

# Save the chart

png("figures/event_study_baseline_inventors.png", 
    width = 20, 
    height = 10, 
    units = "cm",
    res = 800)

iplot(baseline_event_study_inventors)

dev.off()

# Repeat with applicants only

baseline_event_study_applicants <-
    feols(ln_applicants ~ i(periods, treatment_dummy, ref = -1) | province_code + periods,
          data = df_event_study,
          cluster = ~ province_code + periods)

summary(baseline_event_study_applicants)

iplot(baseline_event_study_applicants)

# Repeat with owners only

baseline_event_study_owners <-
    feols(ln_owners ~ i(periods, treatment_dummy, ref = -1) | province_code + periods,
          data = df_event_study,
          cluster = ~ province_code + periods)

summary(baseline_event_study_owners)

iplot(baseline_event_study_owners)
