# R Script: Event Study Regressions for parties as DVs
# ECON899 MA Paper 
# SFU Economics
# Daniel Sanchez
# Spring 2024 

# This script implements event study regressions with parties as dependent variables.

# Preliminaries -----------------------------------------------------------

# Libraries 

library(dplyr, warn.conflicts = FALSE)
library(fixest, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(modelsummary, warn.conflicts = FALSE)
library(forcats, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)
library(gridExtra, warn.conflicts = FALSE)
library(stringr, warn.conflicts = FALSE)
library(janitor, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
library(patchwork, warn.conflicts = FALSE)
library(ggfixest, warn.conflicts = FALSE)

# Load full dataset

df_full <- readRDS("data/full_dataset.rds")

# Define treatment start date

treatment_start_date <- ymd("2016-08-01")

# Define valid start and end dates

start_date <- ymd("2001-08-01")

end_date <- ymd("2021-08-01")

# Define valid start and end periods

start_period <- interval(treatment_start_date, start_date)/months(1)

end_period <- interval(treatment_start_date, end_date)/months(1)

# Data preparation -----------------------------------------------------------

# Filter the data to include only the relevant data (periods and provinces)

df <- 
    df_full %>%
    filter(month_year %>% between(start_date, end_date))

# Create a dataframe which has the treatment dummy, equal to 1 whenever the treatment group is Alberta, and the period dummies.

df_event_study <- 
    df %>% 
    mutate(treatment_dummy = (treatment == "Treatment"))

# Baseline event study regressions -----------------------------------------------------------

es_baseline_all_parties <- 
    feols(ln1interested_parties ~ i(periods, treatment_dummy, ref = -1),
          data = df_event_study,
          fixef = c("province_code", "periods"),
          cluster = ~ province_code + periods)

es_baseline_inventors <-
    feols(ln1inventors ~ i(periods, treatment_dummy, ref = -1),
          data = df_event_study,
          fixef = c("province_code", "periods"),
          cluster = ~ province_code + periods)

es_baseline_applicants <-
    feols(ln1applicants ~ i(periods, treatment_dummy, ref = -1),
          data = df_event_study,
          fixef = c("province_code", "periods"),
          cluster = ~ province_code + periods)
        
es_baseline_owners <-
    feols(ln1owners ~ i(periods, treatment_dummy, ref = -1),
          data = df_event_study,
          fixef = c("province_code", "periods"),
          cluster = ~ province_code + periods)

# Event study plot with ggiplot

event_studies_baseline <- list(es_baseline_all_parties, es_baseline_inventors, es_baseline_applicants, es_baseline_owners)

ggiplot(event_studies_baseline,
        geom_style = "errorbar",
        multi_style = "facet",
        ci.width = 0,
        pt.pch = 0,
        facet_args = list(ncol = 1, scales = "free_y"))