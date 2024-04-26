# R Script: Event Study Plots for patents as DVs, monthly data, patent IPC sections
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

# Estimate the event study regressions (with additional controls only - specification 3)

# Define the base controls 

def_controls <- "+ ln_total_pop + ln_total_full_emp + ln_total_median_wage + cpi + ln_exports_all_countries + ln_imports_all_countries + ln_retail_sales + ln_wholesale_sales + ln_manufacturing_sales + ln1_foreign_parties + ln1_business_insolvencies"

# Define the additional controls

extra_controls <- "+ ln1_travellers + ln1_vehicles + ln_electric_power_generation + ln_average_actual_hours + new_housing_price_index + ln_food_services_receipts + ln_total_avg_tenure"

add_controls <- paste(def_controls, extra_controls)

# Estimate controls using fixest
# Change variable names for facet names to be clean

es_add_controls_A <- 
    feols(fml = paste("patent_sections ~ i(periods, treatment_dummy, ref = -1)", add_controls) %>% as.formula(),
          data = df_event_study %>% rename(patent_sections = ln1_patents_A),
          fixef = c("province_code", "periods"),
          cluster = ~ province_code + periods)

es_add_controls_B <-
    feols(fml = paste("patent_sections ~ i(periods, treatment_dummy, ref = -1)", add_controls) %>% as.formula(),
          data = df_event_study %>% rename(patent_sections = ln1_patents_B),
          fixef = c("province_code", "periods"),
          cluster = ~ province_code + periods)

es_add_controls_C <-
    feols(fml = paste("patent_sections ~ i(periods, treatment_dummy, ref = -1)", add_controls) %>% as.formula(),
          data = df_event_study %>% rename(patent_sections = ln1_patents_C),
          fixef = c("province_code", "periods"),
          cluster = ~ province_code + periods)

es_add_controls_D <-
    feols(fml = paste("patent_sections ~ i(periods, treatment_dummy, ref = -1)", add_controls) %>% as.formula(),
          data = df_event_study %>% rename(patent_sections = ln1_patents_D),
          fixef = c("province_code", "periods"),
          cluster = ~ province_code + periods)

es_add_controls_E <-
    feols(fml = paste("patent_sections ~ i(periods, treatment_dummy, ref = -1)", add_controls) %>% as.formula(),
          data = df_event_study %>% rename(patent_sections = ln1_patents_E),
          fixef = c("province_code", "periods"),
          cluster = ~ province_code + periods)

es_add_controls_F <-
    feols(fml = paste("patent_sections ~ i(periods, treatment_dummy, ref = -1)", add_controls) %>% as.formula(),
          data = df_event_study %>% rename(patent_sections = ln1_patents_F),
          fixef = c("province_code", "periods"),
          cluster = ~ province_code + periods)

es_add_controls_G <-
    feols(fml = paste("patent_sections ~ i(periods, treatment_dummy, ref = -1)", add_controls) %>% as.formula(),
          data = df_event_study %>% rename(patent_sections = ln1_patents_G),
          fixef = c("province_code", "periods"),
          cluster = ~ province_code + periods)

es_add_controls_H <-
    feols(fml = paste("patent_sections ~ i(periods, treatment_dummy, ref = -1)", add_controls) %>% as.formula(),
          data = df_event_study %>% rename(patent_sections = ln1_patents_H),
          fixef = c("province_code", "periods"),
          cluster = ~ province_code + periods)

# Event study plots ------------------------------------------------------------

# Periods 

periods_for_plot <- seq(min(df_event_study$periods), max(df_event_study$periods), by = 12)

# Draw the event study plot for all sections 

event_studies_add_controls_sections <- list(`Section A patents` = es_add_controls_A, 
                                            `Section B patents` = es_add_controls_B, 
                                            `Section C patents` = es_add_controls_C,
                                            `Section D patents` = es_add_controls_D,
                                            `Section E patents` = es_add_controls_E,
                                            `Section F patents` = es_add_controls_F,
                                            `Section G patents` = es_add_controls_G,
                                            `Section H patents` = es_add_controls_H)


event_study_plot_faceted_patent_sections <-
    ggiplot(event_studies_add_controls_sections, 
            geom_style= "ribbon",
            multi_style = "facet",
            ci.width = 0,
            pt.pch = 1,
            col = rep("#0D3692",9),
            facet_args = list(ncol = 2, scales = "free_y")) + 
        theme_bw() +
        labs(title = "", 
            x = "Months to treatment",
            y = "Event study interaction term and 95% C.I.") + 
        scale_x_continuous(breaks = periods_for_plot) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none")

event_study_plot_faceted_patent_sections

# Save the plot

ggsave("figures/event-studies/monthly/patent_sections_faceted.png", 
        event_study_plot_faceted_patent_sections, 
        width = 22.5, 
        height = 12.5, 
        units = "cm", 
        dpi = 800)