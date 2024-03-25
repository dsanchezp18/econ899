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
library(ggfixest)

# Load data

df <- readRDS("data/full_dataset.rds")

# Define a treatment date (month-year the AITC was passed)

treatment_start_date <- ymd("2016-08-01")
treatment_group <- "AB"

# Define "relevant" periods, which are periods for which there is there is relevant data 

start_date <- ymd("1990-01-01")
end_date <- ymd("2022-12-31")

# Data preparation -----------------------------------------------------------

# Create a dataframe which has the treatment dummy, equal to 1 whenever the treatment group is Alberta, and the period dummies.

df_event_study <- 
    df %>% 
    mutate(period_dummies = as_factor(periods) %>% fct_relevel("-1"),
           event_study_dummies = fixest::i(period_dummies, treatment, ref = "-1", ref2 = "Control"),
           treatment_dummy = (treatment == "Treatment"))

# Event studies with controls -----------------------------------------------------------

# Implement event study specifications by interacting the treatment dummy (being Alberta) with all period dummies except one period before the post period started. 

# Define a formula object with the summation of all explanatory variables to be included in the models

explanatory_vars <- "~ log(total_pop) + log(total_emp) + log(total_median_wage) + cpi + log(business_insolvencies+1) + log(travellers) + new_housing_price_index + log(electric_power_generation + 1) + 
log(wages_paid_patenting_ind) + log(emp_patenting_ind) + log(exports_all_countries) + log(imports_all_countries)" %>% 
                    as.formula()

# Estimate the models with the explanatory variables included.

# With all parties -----------------------------------------------------------

event_study_covariates_all_parties <-
    feols(update(explanatory_vars, ln_parties ~ i(periods, treatment_dummy, ref = -1) + .), 
          data = df_event_study,
          fixef = c("province_code", "periods"),
          cluster = ~ province_code + periods)

summary(event_study_covariates_all_parties)

# iplot(event_study_covariates_all_parties, 
#       main = "Event Study Plot",
#       xlab = "Periods",
#       ylab = "Interaction term coefficients with 95% C.I.",
#       sub = "All parties involved in patent applications")


event_study_plot_all_parties <-
      ggiplot(event_study_covariates_all_parties, 
              geom_style= "errorbar",
              ci.width = 1.2,
              col = "#0D3692") + 
      scale_x_continuous(limits = c(-237,80)) +
      theme_minimal() + 
      labs(title = "Event Study Plot",
      x = "Periods",
      y = "Interaction term coefficients with 95% C.I.",
      subtitle = "All parties involved in patent applications")

event_study_plot_all_parties

## With inventors only -----------------------------------------------------------

event_study_covariates_inventors <-
    feols(update(explanatory_vars, ln_inventors ~ i(periods, treatment_dummy, ref = -1) + .), 
          data = (df_event_study %>% filter(periods > -236)),
          fixef = c("province_code", "periods"),
          cluster = ~ province_code + periods)

summary(event_study_covariates_inventors)

# iplot(event_study_covariates_inventors, 
#       main = "Event Study Plot",
#       xlab = "Periods",
#       ylab = "Interaction term coefficients with 95% C.I.",
#       sub = "Inventors involved in patent applications")

# With ggfixest

event_study_plot_inventors <-
      ggiplot(event_study_covariates_inventors, 
              geom_style= "errorbar",
              ci.width = 1.2,
              col = "#0D3692") + 
      scale_x_continuous(limits = c(-237,80)) +
      theme_minimal() + 
      labs(title = "Event Study Plot",
      x = "Periods",
      y = "Interaction term coefficients with 95% C.I.",
      subtitle = "Inventors involved in patent applications")

event_study_plot_inventors

## With applicants only -----------------------------------------------------------

event_study_covariates_applicants <-
    feols(update(explanatory_vars, ln_applicants ~ i(periods, treatment_dummy, ref = -1) + .), 
          data = (df_event_study %>% filter(periods > -236)),
          fixef = c("province_code", "periods"),
          cluster = ~ province_code + periods)

summary(event_study_covariates_applicants)

# iplot(event_study_covariates_applicants, 
#       main = "Event Study Plot",
#       xlab = "Periods",
#       ylab = "Interaction term coefficients with 95% C.I.",
#       sub = "Applicants involved in patent applications")

# With ggfixest

event_study_plot_applicants <-
      ggiplot(event_study_covariates_applicants, 
              geom_style= "errorbar",
              ci.width = 1.2,
              col = "#0D3692") + 
      scale_x_continuous(limits = c(-237,80)) +
      theme_minimal() + 
      labs(title = "Event Study Plot",
      x = "Periods",
      y = "Interaction term coefficients with 95% C.I.",
      subtitle = "Applicants involved in patent applications")

event_study_plot_applicants

## With owners only -----------------------------------------------------------

event_study_covariates_owners <-
    feols(update(explanatory_vars, ln_owners ~ i(periods, treatment_dummy, ref = -1) + .), 
          data = (df_event_study %>% filter(periods > -236)),
          fixef = c("province_code", "periods"),
          cluster = ~ province_code + periods)

summary(event_study_covariates_owners)

# iplot(event_study_covariates_owners, 
#       main = "Event Study Plot",
#       xlab = "Periods",
#       ylab = "Interaction term coefficients with 95% C.I.",
#       sub = "Owners involved in patent applications")  

## Patents filed -----------------------------------------------------------

event_study_patents <-
    feols(update(explanatory_vars, ln_patents_filed ~ i(periods, treatment_dummy, ref = -1) + ln_foreign_parties + .), 
          data = (df_event_study %>% filter(periods > -236)),
          fixef = c("province_code", "periods"),
          cluster = ~ province_code + periods)

summary(event_study_patents)

# iplot(event_study_patents, 
#       main = "Event Study Plot - Patents filed as the dependent variable",
#       xlab = "Periods before the AITC was passed",
#       ylab = "Interaction term coefficients with 95% C.I.")

# With ggfixest

event_study_plot_patents <-
      ggiplot(event_study_patents, 
              geom_style= "errorbar",
              ci.width = 1.2,
              col = "#0D3692") + 
      scale_x_continuous(limits = c(-237,80)) +
      theme_minimal() + 
      labs(title = "Event Study Plot",
      x = "Periods",
      y = "Interaction term coefficients with 95% C.I.",
      subtitle = "Patents filed as the dependent variable")

event_study_plot_patents

# Export the charts ----------------------------------------------------------------

## All parties ----------------------------------------------------------------

ggsave("charts/event_study_plot_all_parties.png", event_study_plot_all_parties, width = 20, height = 6, units = "in", dpi = 800)

## Inventors ----------------------------------------------------------------

ggsave("charts/event_study_plot_inventors.png", event_study_plot_inventors, width = 20, height = 6, units = "in", dpi = 800)

## Applicants ----------------------------------------------------------------

ggsave("charts/event_study_plot_applicants.png", event_study_plot_applicants, width = 20, height = 6, units = "in", dpi = 800)

## Owners ----------------------------------------------------------------

ggsave("charts/event_study_plot_owners.png", event_study_plot_owners, width = 20, height = 6, units = "in", dpi = 800)

## Patents filed ----------------------------------------------------------------

ggsave("charts/event_study_plot_patents.png", event_study_plot_patents, width = 20, height = 6, units = "in", dpi = 800)
