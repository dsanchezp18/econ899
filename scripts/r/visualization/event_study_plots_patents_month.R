# R Script: Event Study Plots for patents as DVs, monthly data
# ECON899 MA Paper 
# SFU Economics
# Daniel Sanchez
# Spring 2024 

# This script implements event study regressions with patents as dependent variables.

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

# Load the data 

df_full <- readRDS("data/full_dataset_monthly.rds")

# Define treatment start date

treatment_start_date <- ymd("2017-01-01")

# Define valid start and end dates

start_date <- ymd("2001-01-01")

end_date <- ymd("2021-06-01")

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

# Event study regressions -----------------------------------------------------------

# Estimate the event study regressions for patents as dependent variables - all three specifications

# Define the base controls 

def_controls <- "+ ln_total_pop + ln_total_full_emp + ln_total_median_wage + cpi + ln_exports_all_countries + ln_imports_all_countries + ln_retail_sales + ln_wholesale_sales + ln_manufacturing_sales + ln1_foreign_parties + ln1_business_insolvencies"

# Define the additional controls

extra_controls <- "+ ln1_travellers + ln1_vehicles + ln_electric_power_generation + ln_average_actual_hours + new_housing_price_index + ln_food_services_receipts + ln_total_avg_tenure"

add_controls <- paste(def_controls, extra_controls)

## Baseline model ------------------------------------------------------------

es_baseline <- 
    feols(ln1_patents_filed ~ i(periods, treatment_dummy, ref = -1),
          data = df_event_study,
          fixef = c("province_code", "periods"),
          cluster = ~ province_code + periods)

## Defendable controls ------------------------------------------------------------

es_def_controls <- 
    feols(fml = paste("ln1_patents_filed ~ i(periods, treatment_dummy, ref = -1)", def_controls) %>% as.formula(),
          data = df_event_study,
          fixef = c("province_code", "periods"),
          cluster = ~ province_code + periods)

## Additional controls ------------------------------------------------------------

es_add_controls <- 
    feols(fml = paste("ln1_patents_filed ~ i(periods, treatment_dummy, ref = -1)", add_controls) %>% as.formula(),
          data = df_event_study,
          fixef = c("province_code", "periods"),
          cluster = ~ province_code + periods)

# Event study plot ------------------------------------------------------------

# Periods 

periods_for_plot <- seq(min(df_event_study$periods), max(df_event_study$periods), by = 12)

patents_event_studies <- list(`(1) Baseline` = es_baseline,
                              `(2) Economic controls` = es_def_controls, 
                              `(3) Additional controls` = es_add_controls)

event_study_plot_faceted_patents <-
    ggiplot(patents_event_studies,
            geom_style = "errorbar",
            multi_style = "facet",
            ci.width = 0.2,
            pt.pch = 0,
            col = c("#0D3692","#0D3692","#0D3692"),
            ref.line.par = list(col = "grey20"),
            facet_args = list(ncol = 1, scales = "free_x")) +
    theme_bw() +
    labs(title = "", 
         x = "Periods to treatment",
         y = "Event study interaction term and 95% C.I.") +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) + 
    scale_x_continuous(breaks = periods_for_plot, 
                       limits = c(min(periods_for_plot), max(periods_for_plot)),
                       expand = c(0,2)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.ticks.x = element_line(color = "grey50"),
          legend.position = "none")

event_study_plot_faceted_patents

ggsave("figures/event-studies/monthly/patents_faceted.png", width = 22.5, height = 12.5, units = "cm", dpi = 800)