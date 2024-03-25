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

# Define "relevant" periods, a shorter period where I don't see that much stuff happening before the treatment date

start_date <- ymd("2006-01-01")
end_date <- ymd("2021-12-01")

# Data preparation -----------------------------------------------------------

# Create a dataframe which has the treatment dummy, equal to 1 whenever the treatment group is Alberta, and the period dummies.

df_event_study <- 
    df %>% 
    mutate(period_dummies = as_factor(periods) %>% fct_relevel("-1"),
           event_study_dummies = fixest::i(period_dummies, treatment, ref = "-1", ref2 = "Control"),
           treatment_dummy = (treatment == "Treatment"))

# Event studies with controls with all available data -----------------------------------------------------------

# Implement event study specifications by interacting the treatment dummy (being Alberta) with all period dummies except one period before the post period started. 

# Define a formula object with the summation of all explanatory variables to be included in the models

explanatory_vars <- "~ log(total_pop) + log(total_emp) + log(average_actual_hours) + log(total_median_wage) + cpi + log(business_insolvencies) + log(travellers) + log(vehicles) + new_housing_price_index + log(electric_power_generation + 1) + 
log(wages_paid_patenting_ind) + log(emp_patenting_ind) + log(exports_all_countries) + log(imports_all_countries) + log(manufacturing_sales) + log(wholesale_sales) + log(retail_sales)" %>% 
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

# Define a date sequence for the plot

periods_vector <- seq(-236, 70, by = 12)

# Find the months in df which correspond to the periods

months_in_df <- 
      df_event_study %>% 
      select(month_year, periods) %>% 
      filter(periods %in% periods_vector) %>%
      distinct(month_year) %>% 
      pull(month_year) %>%
      format("%b-%Y")

event_study_plot_all_parties <-
      ggiplot(event_study_covariates_all_parties, 
              geom_style= "errorbar",
              ci.width = 1.2,
              col = "#0D3692",
              pt.pch = 0) + 
      scale_x_continuous(limits = c(min(periods_vector),max(periods_vector)), breaks = periods_vector, labels = months_in_df) +
      theme_bw() + 
      labs(title = "Event Study Plot",
      x = "Periods",
      y = "Interaction term coefficients with 95% C.I.",
      subtitle = "All parties involved in patent applications") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

event_study_plot_all_parties

ggsave("figures/event-studies/event_study_plot_all_parties.png", event_study_plot_all_parties, width = 20, height = 6, units = "in", dpi = 800)

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
      theme_bw() + 
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
      theme_bw() + 
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

# With ggfixest

event_study_plot_owners <-
      ggiplot(event_study_covariates_owners, 
              geom_style= "errorbar",
              ci.width = 1.2,
              col = "#0D3692") + 
      scale_x_continuous(limits = c(-237,80)) +
      theme_bw() + 
      labs(title = "Event Study Plot",
      x = "Periods",
      y = "Interaction term coefficients with 95% C.I.",
      subtitle = "Owners involved in patent applications")

event_study_plot_owners

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
      theme_bw() + 
      labs(title = "Event Study Plot",
      x = "Periods",
      y = "Interaction term coefficients with 95% C.I.",
      subtitle = "Patents filed as the dependent variable")

event_study_plot_patents

# Faceted chart with all of the event studies --------------------------------

# List of the event study regressions -----------------------------------------------------------

event_studies <- list(event_study_covariates_all_parties, 
                      event_study_covariates_inventors, 
                      event_study_covariates_applicants, 
                      event_study_covariates_owners, 
                      event_study_patents)

event_study_all <-
      ggiplot(event_studies, 
              geom_style= "errorbar",
              multi_style = "facet",
              ci.width = 0,
              pt.pch = 0,
              facet_args = list(ncol = 1, scales = "free_y")) +
      scale_x_continuous(limits = c(min(periods_vector),max(periods_vector)), breaks = periods_vector, labels = months_in_df) +
      theme_bw() + 
      labs(title = "Event Study Plot",
           x = "Periods",
           y = "Interaction term coefficients with 95% C.I.") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none")

event_study_all

# Event study plots with relevant periods --------------------------------

# Define the dataframe with the relevant periods

df_event_study_relevant <- 
    df %>% 
    filter(month_year %>% between(start_date, end_date)) %>% 
    mutate(period_dummies = as_factor(periods) %>% fct_relevel("-1"),
           event_study_dummies = fixest::i(period_dummies, treatment, ref = "-1", ref2 = "Control"),
           treatment_dummy = (treatment == "Treatment"))

# Reestimate all five event studies with this dataframe

## With all parties -----------------------------------------------------------

event_study_covariates_all_parties_relevant <-
    feols(update(explanatory_vars, ln_parties ~ i(periods, treatment_dummy, ref = -1) + .), 
          data = df_event_study_relevant,
          fixef = c("province_code", "periods"),
          cluster = ~ province_code + periods)

## With inventors only -----------------------------------------------------------

event_study_covariates_inventors_relevant <-
    feols(update(explanatory_vars, ln_inventors ~ i(periods, treatment_dummy, ref = -1) + .), 
          data = df_event_study_relevant,
          fixef = c("province_code", "periods"),
          cluster = ~ province_code + periods)

## With applicants only -----------------------------------------------------------

event_study_covariates_applicants_relevant <-
    feols(update(explanatory_vars, ln_applicants ~ i(periods, treatment_dummy, ref = -1) + .), 
          data = df_event_study_relevant,
          fixef = c("province_code", "periods"),
          cluster = ~ province_code + periods)

## With owners only -----------------------------------------------------------

event_study_covariates_owners_relevant <-
    feols(update(explanatory_vars, ln_owners ~ i(periods, treatment_dummy, ref = -1) + .), 
          data = df_event_study_relevant,
          fixef = c("province_code", "periods"),
          cluster = ~ province_code + periods)

## Patents filed -----------------------------------------------------------

event_study_patents_relevant <-
    feols(update(explanatory_vars, ln_patents_filed ~ i(periods, treatment_dummy, ref = -1) + ln_foreign_parties + .), 
          data = df_event_study_relevant,
          fixef = c("province_code", "periods"),
          cluster = ~ province_code + periods)

## Event study charts for relevant periods -----------------------------------------------------------

# List of the event study regressions -----------------------------------------------------------

event_studies_relevant <- list(event_study_covariates_all_parties_relevant, 
                               event_study_covariates_inventors_relevant, 
                               event_study_covariates_applicants_relevant, 
                               event_study_covariates_owners_relevant, 
                               event_study_patents_relevant)

# Do it faceted

# Define a date sequence for the plot

periods_vector <- seq(-126, 66, by = 12)

# Find the months in df which correspond to the periods

months_in_df <- 
      df_event_study %>% 
      select(month_year, periods) %>% 
      filter(periods %in% periods_vector) %>%
      distinct(month_year) %>% 
      pull(month_year) %>%
      c(as.Date("2021-08-01")) %>% 
      format("%b-%Y")

event_study_relevant <-
      ggiplot(event_studies_relevant, 
              geom_style= "errorbar",
              multi_style = "facet",
              ci.width = 0,
              pt.pch = 0,
              facet_args = list(ncol = 1, scales = "free_y")) +
      scale_x_continuous(limits = c(min(periods_vector),max(periods_vector)), breaks = periods_vector, labels = months_in_df) +
      theme_bw() + 
      labs(title = "Event Study Plot",
           x = "Periods",
           y = "Interaction term coefficients with 95% C.I.") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none")

# Section codes ----------------------------------------------------------------

# Reimplement event studies with the patents per section code

# Same formula, but ln_patents_A

event_study_A <-
    feols(update(explanatory_vars, ln_patents_A ~ i(periods, treatment_dummy, ref = -1) + .), 
          data = df_event_study,
          fixef = c("province_code", "periods"),
          cluster = ~ province_code + periods)

summary(event_study_A)

periods_vector <- seq(-236, 70, by = 12)

# Find the months in df which correspond to the periods

months_in_df <- 
      df_event_study %>% 
      select(month_year, periods) %>% 
      filter(periods %in% periods_vector) %>%
      distinct(month_year) %>% 
      pull(month_year) %>%
      format("%b-%Y")

event_study_plot_A <-
      ggiplot(event_study_covariates_all_parties, 
              geom_style= "errorbar",
              ci.width = 1.2,
              col = "#0D3692",
              pt.pch = 0) + 
      scale_x_continuous(limits = c(min(periods_vector),max(periods_vector)), breaks = periods_vector, labels = months_in_df) +
      theme_bw() + 
      labs(title = "Event Study Plot",
      x = "Periods",
      y = "Interaction term coefficients with 95% C.I.",
      subtitle = "Patents with section code A") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

event_study_plot_A

ggsave("figures/event-studies/event_study_plot_A.png", event_study_plot_A, width = 20, height = 6, units = "in", dpi = 800)

# Same formula, but ln_patents_B

event_study_B <-
    feols(update(explanatory_vars, ln_patents_B ~ i(periods, treatment_dummy, ref = -1) + .), 
          data = df_event_study,
          fixef = c("province_code", "periods"),
          cluster = ~ province_code + periods)

summary(event_study_B)

event_study_plot_B <-
      ggiplot(event_study_covariates_all_parties, 
              geom_style= "errorbar",
              ci.width = 1.2,
              col = "#0D3692",
              pt.pch = 0) + 
      scale_x_continuous(limits = c(min(periods_vector),max(periods_vector)), breaks = periods_vector, labels = months_in_df) +
      theme_bw() + 
      labs(title = "Event Study Plot",
      x = "Periods",
      y = "Interaction term coefficients with 95% C.I.",
      subtitle = "Patents with section code B") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

event_study_plot_B

ggsave("figures/event-studies/event_study_plot_B.png", event_study_plot_B, width = 20, height = 6, units = "in", dpi = 800)

# Same formula, but ln_patents_C

event_study_C <-
    feols(update(explanatory_vars, ln_patents_C ~ i(periods, treatment_dummy, ref = -1) + .), 
          data = df_event_study,
          fixef = c("province_code", "periods"),
          cluster = ~ province_code + periods)

summary(event_study_C)

event_study_plot_C <-
      ggiplot(event_study_covariates_all_parties, 
              geom_style= "errorbar",
              ci.width = 1.2,
              col = "#0D3692",
              pt.pch = 0) + 
      scale_x_continuous(limits = c(min(periods_vector),max(periods_vector)), breaks = periods_vector, labels = months_in_df) +
      theme_bw() + 
      labs(title = "Event Study Plot",
      x = "Periods",
      y = "Interaction term coefficients with 95% C.I.",
      subtitle = "Patents with section code C") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

event_study_plot_C

ggsave("figures/event-studies/event_study_plot_C.png", event_study_plot_C, width = 20, height = 6, units = "in", dpi = 800)

# Same formula, but ln_patents_D

event_study_D <-
    feols(update(explanatory_vars, ln_patents_D ~ i(periods, treatment_dummy, ref = -1) + .), 
          data = df_event_study,
          fixef = c("province_code", "periods"),
          cluster = ~ province_code + periods)

summary(event_study_D)

event_study_plot_D <-
      ggiplot(event_study_covariates_all_parties, 
              geom_style= "errorbar",
              ci.width = 1.2,
              col = "#0D3692",
              pt.pch = 0) + 
      scale_x_continuous(limits = c(min(periods_vector),max(periods_vector)), breaks = periods_vector, labels = months_in_df) +
      theme_bw() + 
      labs(title = "Event Study Plot",
      x = "Periods",
      y = "Interaction term coefficients with 95% C.I.",
      subtitle = "Patents with section code D") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

event_study_plot_D

ggsave("figures/event-studies/event_study_plot_D.png", event_study_plot_D, width = 20, height = 6, units = "in", dpi = 800)

# Same formula, but ln_patents_E

event_study_E <-
    feols(update(explanatory_vars, ln_patents_E ~ i(periods, treatment_dummy, ref = -1) + .), 
          data = df_event_study,
          fixef = c("province_code", "periods"),
          cluster = ~ province_code + periods)

summary(event_study_E)

event_study_plot_E <-
      ggiplot(event_study_covariates_all_parties, 
              geom_style= "errorbar",
              ci.width = 1.2,
              col = "#0D3692",
              pt.pch = 0) + 
      scale_x_continuous(limits = c(min(periods_vector),max(periods_vector)), breaks = periods_vector, labels = months_in_df) +
      theme_bw() + 
      labs(title = "Event Study Plot",
      x = "Periods",
      y = "Interaction term coefficients with 95% C.I.",
      subtitle = "Patents with section code E") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

event_study_plot_E

ggsave("figures/event-studies/event_study_plot_E.png", event_study_plot_E, width = 20, height = 6, units = "in", dpi = 800)

# Same formula, but ln_patents_F

event_study_F <-
    feols(update(explanatory_vars, ln_patents_F ~ i(periods, treatment_dummy, ref = -1) + .), 
          data = df_event_study,
          fixef = c("province_code", "periods"),
          cluster = ~ province_code + periods)

summary(event_study_F)

event_study_plot_F <-
      ggiplot(event_study_covariates_all_parties, 
              geom_style= "errorbar",
              ci.width = 1.2,
              col = "#0D3692",
              pt.pch = 0) + 
      scale_x_continuous(limits = c(min(periods_vector),max(periods_vector)), breaks = periods_vector, labels = months_in_df) +
      theme_bw() + 
      labs(title = "Event Study Plot",
      x = "Periods",
      y = "Interaction term coefficients with 95% C.I.",
      subtitle = "Patents with section code F") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

event_study_plot_F

ggsave("figures/event-studies/event_study_plot_F.png", event_study_plot_F, width = 20, height = 6, units = "in", dpi = 800)

# Same formula, but ln_patents_G

event_study_G <-
    feols(update(explanatory_vars, ln_patents_G ~ i(periods, treatment_dummy, ref = -1) + .), 
          data = df_event_study,
          fixef = c("province_code", "periods"),
          cluster = ~ province_code + periods)

summary(event_study_G)

event_study_plot_G <-
      ggiplot(event_study_covariates_all_parties, 
              geom_style= "errorbar",
              ci.width = 1.2,
              col = "#0D3692",
              pt.pch = 0) + 
      scale_x_continuous(limits = c(min(periods_vector),max(periods_vector)), breaks = periods_vector, labels = months_in_df) +
      theme_bw() + 
      labs(title = "Event Study Plot",
      x = "Periods",
      y = "Interaction term coefficients with 95% C.I.",
      subtitle = "Patents with section code G") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

event_study_plot_G

ggsave("figures/event-studies/event_study_plot_G.png", event_study_plot_G, width = 20, height = 6, units = "in", dpi = 800)

# Same formula, but ln_patents_H

event_study_H <-
    feols(update(explanatory_vars, ln_patents_H ~ i(periods, treatment_dummy, ref = -1) + .), 
          data = df_event_study,
          fixef = c("province_code", "periods"),
          cluster = ~ province_code + periods)

summary(event_study_H)

event_study_plot_H <-
      ggiplot(event_study_covariates_all_parties, 
              geom_style= "errorbar",
              ci.width = 1.2,
              col = "#0D3692",
              pt.pch = 0) + 
      scale_x_continuous(limits = c(min(periods_vector),max(periods_vector)), breaks = periods_vector, labels = months_in_df) +
      theme_bw() + 
      labs(title = "Event Study Plot",
      x = "Periods",
      y = "Interaction term coefficients with 95% C.I.",
      subtitle = "Patents with section code H") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

event_study_plot_H

ggsave("figures/event-studies/event_study_plot_H.png", event_study_plot_H, width = 20, height = 6, units = "in", dpi = 800)

# Same formula, but ln_patents_M

event_study_M <-
    feols(update(explanatory_vars, ln_patents_M ~ i(periods, treatment_dummy, ref = -1) + .), 
          data = df_event_study,
          fixef = c("province_code", "periods"),
          cluster = ~ province_code + periods)

summary(event_study_M)

event_study_plot_M <-
      ggiplot(event_study_covariates_all_parties, 
              geom_style= "errorbar",
              ci.width = 1.2,
              col = "#0D3692",
              pt.pch = 0) + 
      scale_x_continuous(limits = c(min(periods_vector),max(periods_vector)), breaks = periods_vector, labels = months_in_df) +
      theme_bw() + 
      labs(title = "Event Study Plot",
      x = "Periods",
      y = "Interaction term coefficients with 95% C.I.",
      subtitle = "Patents with section code M") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

event_study_plot_M

ggsave("figures/event-studies/event_study_plot_M.png", event_study_plot_M, width = 20, height = 6, units = "in", dpi = 800)

# Faceted chart with all of the event studies with sections --------------------------------

# List of the event study regressions -----------------------------------------------------------

event_studies_section_codes <- list(event_study_A, 
                                    event_study_B, 
                                    event_study_C, 
                                    event_study_E, 
                                    event_study_F, 
                                    event_study_G, 
                                    event_study_H, 
                                    event_study_M)

event_study_all_sections <-
      ggiplot(event_studies_section_codes, 
              geom_style= "errorbar",
              multi_style = "facet",
              ci.width = 0,
              pt.pch = 1,
              col = rep("#0D3692",9),
              facet_args = list(ncol = 2, scales = "free_y")) +
      scale_x_continuous(limits = c(min(periods_vector),max(periods_vector)), breaks = periods_vector, labels = months_in_df) +
      theme_bw() + 
      labs(title = "Event Study Plot",
           x = "Periods",
           y = "Interaction term coefficients with 95% C.I.") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none")

event_study_all_sections

ggsave("figures/event-studies/event_study_all_sections.png", event_study_all_sections, width = 20, height = 20, units = "in", dpi = 800)

