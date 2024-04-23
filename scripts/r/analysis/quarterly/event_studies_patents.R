# R Script: Event Study Regressions for patents as DVs
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

df_full <- readRDS("data/full_data_quarterly.rds")

# Define treatment start date

treatment_start_date <- ymd("2016-04-01")

# Define valid start and end dates

start_date <- ymd("2001-01-01")

end_date <- ymd("2021-06-01")

# Get quarter dates with floor_date

start_date_quarter <- floor_date(start_date, "quarter")

end_date_quarter <- floor_date(end_date, "quarter")

treatment_start_date_quarter <- floor_date(treatment_start_date, "quarter")

# Define valid start and end periods

start_period <- interval(treatment_start_date_quarter, start_date_quarter)/months(3)

end_period <- interval(treatment_start_date_quarter, end_date_quarter)/months(3)

# Data preparation -----------------------------------------------------------

# Filter the data to include only the relevant data (periods and provinces)

df <- 
    df_full %>%
    filter(quarter_year_date %>% between(start_date_quarter, end_date_quarter))

# Create a dataframe which has the treatment dummy, equal to 1 whenever the treatment group is Alberta, and the period dummies.

df_event_study <- 
    df %>% 
    mutate(treatment_dummy = (treatment == "Treatment"))

# Event study regressions -----------------------------------------------------------

## Baseline model ------------------------------------------------------------

es_baseline <- 
    feols(ln1_patents_filed ~ i(periods, treatment_dummy, ref = -1),
          data = df_event_study,
          fixef = c("province_code", "periods"),
          cluster = ~ province_code + periods)

## Defendable controls ------------------------------------------------------------

def_controls <- "+ ln_total_pop + ln_total_full_emp + ln_total_median_wage + cpi + ln_exports_all_countries + ln_imports_all_countries + ln_retail_sales + ln_wholesale_sales + ln_manufacturing_sales + ln1_foreign_parties + ln1_business_insolvencies"

es_def_controls <- 
    feols(fml = paste("ln1_patents_filed ~ i(periods, treatment_dummy, ref = -1)", def_controls) %>% as.formula(),
          data = df_event_study,
          fixef = c("province_code", "periods"),
          cluster = ~ province_code + periods)

## Additional controls ------------------------------------------------------------

extra_controls <- "+ ln1_travellers + ln1_vehicles + ln_electric_power_generation + ln_average_actual_hours + new_housing_price_index + ln_food_services_receipts + ln_total_avg_tenure"

add_controls <- paste(def_controls, extra_controls)

es_add_controls <- 
    feols(fml = paste("ln1_patents_filed ~ i(periods, treatment_dummy, ref = -1)", add_controls) %>% as.formula(),
          data = df_event_study,
          fixef = c("province_code", "periods"),
          cluster = ~ province_code + periods)

# Event study plots ------------------------------------------------------------

# Periods 

periods_for_plot <- seq(start_period, end_period, by = 4)

# Dates 

dates <- 
    df_event_study %>%
    filter(periods %in% periods_for_plot) %>% 
    pull(quarter_year) %>%
    unique()

## Baseline event study plots ------------------------------------------------------------

# Event study plot with ggiplot

ggiplot(es_baseline,
        geom_style = "errorbar",
        ci.width = 0.95,
        col = "#0D3692",
        pt.pch = 0) +
theme_bw() + 
scale_x_continuous(breaks = periods_for_plot, labels = dates) +
theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("figures/event-studies/quarterly/patents_baseline.png", width = 17, height = 10, units = "cm", dpi = 800)

### Defendable controls ------------------------------------------------------------

ggiplot(es_def_controls,
        geom_style = "errorbar",
        ci.width = 0.95,
        col = "#0D3692",
        pt.pch = 0) +
theme_bw() +
scale_x_continuous(breaks = periods_for_plot, labels = dates) +
theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("figures/event-studies/quarterly/patents_def_controls.png", width = 17, height = 10, units = "cm", dpi = 800)

### Additional controls ------------------------------------------------------------

ggiplot(es_add_controls,
        geom_style = "errorbar",
        ci.width = 0.95,
        col = "#0D3692",
        pt.pch = 0) +
theme_bw() +
scale_x_continuous(breaks = periods_for_plot, labels = dates) +
theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("figures/event-studies/quarterly/patents_add_controls.png", width = 17, height = 10, units = "cm", dpi = 800)

### All three together with facets ------------------------------------------------------------

patents_event_studies <- list(`(1) Baseline` = es_baseline,
                              `(2) Economic controls` = es_def_controls, 
                              `(3) Additional controls` = es_add_controls)

event_study_plot_faceted_patents <-
    ggiplot(patents_event_studies,
            geom_style = "errorbar",
            multi_style = "facet",
            ci.width = 0.1,
            pt.pch = 0,
            col = c("#0D3692","#0D3692","#0D3692"),
            facet_args = list(ncol = 1, scales = "free_y")) +
    theme_bw() +
    labs(title = "", 
         x = "Quarter-year",
         y = "Event study interaction term and 95% C.I.") + 
    scale_x_continuous(breaks = periods_for_plot, labels = dates) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

event_study_plot_faceted_patents

ggsave("figures/event-studies/quarterly/patents_faceted.png", width = 15, height = 17, units = "cm", dpi = 800)


# Patent sections ------------------------------------------------------------

## Baseline ------------------------------------------------------------

es_baseline_A <- 
    feols(fml = ln1_patents_A ~ i(periods, treatment_dummy, ref = -1),
          data = df_event_study,
          fixef = c("province_code", "periods"),
          cluster = ~ province_code + periods
    )

es_baseline_B <-
    feols(fml = ln1_patents_B ~ i(periods, treatment_dummy, ref = -1),
          data = df_event_study,
          fixef = c("province_code", "periods"),
          cluster = ~ province_code + periods
    )

es_baseline_C <-
    feols(fml = ln1_patents_C ~ i(periods, treatment_dummy, ref = -1),
          data = df_event_study,
          fixef = c("province_code", "periods"),
          cluster = ~ province_code + periods
    )

es_baseline_D <-
    feols(fml = ln1_patents_D ~ i(periods, treatment_dummy, ref = -1),
          data = df_event_study,
          fixef = c("province_code", "periods"),
          cluster = ~ province_code + periods
    )

es_baseline_E <-
    feols(fml = ln1_patents_E ~ i(periods, treatment_dummy, ref = -1),
          data = df_event_study,
          fixef = c("province_code", "periods"),
          cluster = ~ province_code + periods
    )

es_baseline_F <-
    feols(fml = ln1_patents_F ~ i(periods, treatment_dummy, ref = -1),
          data = df_event_study,
          fixef = c("province_code", "periods"),
          cluster = ~ province_code + periods
    )

es_baseline_G <-
    feols(fml = ln1_patents_G ~ i(periods, treatment_dummy, ref = -1),
          data = df_event_study,
          fixef = c("province_code", "periods"),
          cluster = ~ province_code + periods
    )

es_baseline_H <-
    feols(fml = ln1_patents_H ~ i(periods, treatment_dummy, ref = -1),
          data = df_event_study,
          fixef = c("province_code", "periods"),
          cluster = ~ province_code + periods
    )

es_baseline_Multiple <-
    feols(fml = ln1_patents_Multiple ~ i(periods, treatment_dummy, ref = -1),
          data = df_event_study,
          fixef = c("province_code", "periods"),
          cluster = ~ province_code + periods
    )

## Defendable controls ------------------------------------------------------------

es_def_controls_A <- 
    feols(fml = paste("ln1_patents_A ~ i(periods, treatment_dummy, ref = -1)", def_controls) %>% as.formula(),
          data = df_event_study,
          fixef = c("province_code", "periods"),
          cluster = ~ province_code + periods)
    
es_def_controls_B <-
    feols(fml = paste("ln1_patents_B ~ i(periods, treatment_dummy, ref = -1)", def_controls) %>% as.formula(),
          data = df_event_study,
          fixef = c("province_code", "periods"),
          cluster = ~ province_code + periods)
        
es_def_controls_C <-
    feols(fml = paste("ln1_patents_C ~ i(periods, treatment_dummy, ref = -1)", def_controls) %>% as.formula(),
          data = df_event_study,
          fixef = c("province_code", "periods"),
          cluster = ~ province_code + periods)
    
es_def_controls_D <-
    feols(fml = paste("ln1_patents_D ~ i(periods, treatment_dummy, ref = -1)", def_controls) %>% as.formula(),
          data = df_event_study,
          fixef = c("province_code", "periods"),
          cluster = ~ province_code + periods)
    
es_def_controls_E <-
    feols(fml = paste("ln1_patents_E ~ i(periods, treatment_dummy, ref = -1)", def_controls) %>% as.formula(),
          data = df_event_study,
          fixef = c("province_code", "periods"),
          cluster = ~ province_code + periods)

es_def_controls_F <-
    feols(fml = paste("ln1_patents_F ~ i(periods, treatment_dummy, ref = -1)", def_controls) %>% as.formula(),
          data = df_event_study,
          fixef = c("province_code", "periods"),
          cluster = ~ province_code + periods)
        
es_def_controls_G <-
    feols(fml = paste("ln1_patents_G ~ i(periods, treatment_dummy, ref = -1)", def_controls) %>% as.formula(),
          data = df_event_study,
          fixef = c("province_code", "periods"),
          cluster = ~ province_code + periods)

es_def_controls_H <-
    feols(fml = paste("ln1_patents_H ~ i(periods, treatment_dummy, ref = -1)", def_controls) %>% as.formula(),
          data = df_event_study,
          fixef = c("province_code", "periods"),
          cluster = ~ province_code + periods)

es_def_controls_Multiple <-
    feols(fml = paste("ln1_patents_Multiple ~ i(periods, treatment_dummy, ref = -1)", def_controls) %>% as.formula(),
          data = df_event_study,
          fixef = c("province_code", "periods"),
          cluster = ~ province_code + periods)

## Additional controls ------------------------------------------------------------

es_add_controls_A <- 
    feols(fml = paste("ln1_patents_A ~ i(periods, treatment_dummy, ref = -1)", add_controls) %>% as.formula(),
          data = df_event_study,
          fixef = c("province_code", "periods"),
          cluster = ~ province_code + periods)

es_add_controls_B <-
    feols(fml = paste("ln1_patents_B ~ i(periods, treatment_dummy, ref = -1)", add_controls) %>% as.formula(),
          data = df_event_study,
          fixef = c("province_code", "periods"),
          cluster = ~ province_code + periods)

es_add_controls_C <-
    feols(fml = paste("ln1_patents_C ~ i(periods, treatment_dummy, ref = -1)", add_controls) %>% as.formula(),
          data = df_event_study,
          fixef = c("province_code", "periods"),
          cluster = ~ province_code + periods)
    
es_add_controls_D <-
    feols(fml = paste("ln1_patents_D ~ i(periods, treatment_dummy, ref = -1)", add_controls) %>% as.formula(),
          data = df_event_study,
          fixef = c("province_code", "periods"),
          cluster = ~ province_code + periods)

es_add_controls_E <-
    feols(fml = paste("ln1_patents_E ~ i(periods, treatment_dummy, ref = -1)", add_controls) %>% as.formula(),
          data = df_event_study,
          fixef = c("province_code", "periods"),
          cluster = ~ province_code + periods)

es_add_controls_F <-
    feols(fml = paste("ln1_patents_F ~ i(periods, treatment_dummy, ref = -1)", add_controls) %>% as.formula(),
          data = df_event_study,
          fixef = c("province_code", "periods"),
          cluster = ~ province_code + periods)

es_add_controls_G <-
    feols(fml = paste("ln1_patents_G ~ i(periods, treatment_dummy, ref = -1)", add_controls) %>% as.formula(),
          data = df_event_study,
          fixef = c("province_code", "periods"),
          cluster = ~ province_code + periods)

es_add_controls_H <-
    feols(fml = paste("ln1_patents_H ~ i(periods, treatment_dummy, ref = -1)", add_controls) %>% as.formula(),
          data = df_event_study,
          fixef = c("province_code", "periods"),
          cluster = ~ province_code + periods)

es_add_controls_Multiple <-
    feols(fml = paste("ln1_patents_Multiple ~ i(periods, treatment_dummy, ref = -1)", add_controls) %>% as.formula(),
          data = df_event_study,
          fixef = c("province_code", "periods"),
          cluster = ~ province_code + periods)

# Event study plots ------------------------------------------------------------

## Baseline ------------------------------------------------------------

event_studies_baseline_sections <- list(
    es_baseline_A, es_baseline_B, es_baseline_C, es_baseline_D, es_baseline_E, es_baseline_F, es_baseline_G, es_baseline_H, es_baseline_Multiple
)

ggiplot(event_studies_baseline_sections, 
        geom_style= "ribbon",
        multi_style = "facet",
        ci.width = 0,
        pt.pch = 1,
        facet_args = list(ncol = 2, scales = "free_y"))

## Defendable controls ------------------------------------------------------------

event_studies_def_controls_sections <- list(
    es_def_controls_A, es_def_controls_B, es_def_controls_C, es_def_controls_D, es_def_controls_E, es_def_controls_F, es_def_controls_G, es_def_controls_H, es_def_controls_Multiple
)

ggiplot(event_studies_def_controls_sections, 
        geom_style= "ribbon",
        multi_style = "facet",
        ci.width = 0,
        pt.pch = 1,
        facet_args = list(ncol = 2, scales = "free_y")) + 
theme_bw() +
scale_x_continuous(breaks = periods_for_plot, labels = dates) +
theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("figures/event-studies/quarterly/patents_def_controls_by_section.png", width = 17, height = 22, units = "cm", dpi = 800)

## Additional controls ------------------------------------------------------------

event_studies_add_controls_sections <- list(
    es_add_controls_A, es_add_controls_B, es_add_controls_C, es_add_controls_D, es_add_controls_E, es_add_controls_F, es_add_controls_G, es_add_controls_H, es_add_controls_Multiple
)

ggiplot(event_studies_add_controls_sections, 
        geom_style= "ribbon",
        multi_style = "facet",
        ci.width = 0,
        pt.pch = 1,
        facet_args = list(ncol = 2, scales = "free_y"))

## Only for patents A ------------------------------------------------------------

event_studies_patents_A <- list(es_baseline_A, es_def_controls_A, es_add_controls_A)

ggiplot(event_studies_patents_A, 
        geom_style= "errorbar",
        multi_style = "facet",
        ci.width = 0,
        pt.pch = 1,
        facet_args = list(ncol = 1, scales = "free_y")) +
theme_bw() +
scale_x_continuous(breaks = periods_for_plot, labels = dates) +
theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
labs(title = "Patents A: Human Necessities")

ggsave("figures/event-studies/quarterly/patents_def_controls_by_section.png", width = 17, height = 10, units = "cm", dpi = 800)

# Only for patents B ------------------------------------------------------------

event_studies_patents_B <- list(es_baseline_B, es_def_controls_B, es_add_controls_B)

ggiplot(event_studies_patents_B, 
        geom_style= "errorbar",
        multi_style = "facet",
        ci.width = 0,
        pt.pch = 1,
        facet_args = list(ncol = 1, scales = "free_y")) +
theme_bw() +
scale_x_continuous(breaks = periods_for_plot, labels = dates) +
theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
labs(title = "Patents B: Performing Operations - Transporting")

# Only for patents C ------------------------------------------------------------

event_studies_patents_C <- list(es_baseline_C, es_def_controls_C, es_add_controls_C)

ggiplot(event_studies_patents_C, 
        geom_style= "errorbar",
        multi_style = "facet",
        ci.width = 0,
        pt.pch = 1,
        facet_args = list(ncol = 1, scales = "free_y")) +
theme_bw() +
scale_x_continuous(breaks = periods_for_plot, labels = dates) +
theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("figures/event-studies/quarterly/patents_def_controls_by_section.png", width = 17, height = 10, units = "cm", dpi = 800)

## Only for patents D ------------------------------------------------------------

event_studies_patents_D <- list(es_baseline_D, es_def_controls_D, es_add_controls_D)

ggiplot(event_studies_patents_D, 
        geom_style= "errorbar",
        multi_style = "facet",
        ci.width = 0,
        pt.pch = 1,
        facet_args = list(ncol = 1, scales = "free_y")) +
theme_bw() +
scale_x_continuous(breaks = periods_for_plot, labels = dates) +
theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
labs(title = "Patents D: Textiles - Paper")

ggsave("figures/event-studies/quarterly/patents_def_controls_by_section.png", width = 17, height = 10, units = "cm", dpi = 800)

## Only for patents E ------------------------------------------------------------

event_studies_patents_E <- list(es_baseline_E, es_def_controls_E, es_add_controls_E)

ggiplot(event_studies_patents_E, 
        geom_style= "errorbar",
        multi_style = "facet",
        ci.width = 0,
        pt.pch = 1,
        facet_args = list(ncol = 1, scales = "free_y")) +
theme_bw() +
scale_x_continuous(breaks = periods_for_plot, labels = dates) +
theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
labs(title = "Patents E: Fixed Constructions")

ggsave("figures/event-studies/quarterly/patents_def_controls_by_section.png", width = 17, height = 10, units = "cm", dpi = 800)

## Only for patents Multiple ------------------------------------------------------------

event_studies_patents_Multiple <- list(es_baseline_Multiple, es_def_controls_Multiple, es_add_controls_Multiple)

ggiplot(event_studies_patents_Multiple, 
        geom_style= "errorbar",
        multi_style = "facet",
        ci.width = 0,
        pt.pch = 1,
        facet_args = list(ncol = 1, scales = "free_y")) +
theme_bw() +
scale_x_continuous(breaks = periods_for_plot, labels = dates) +
theme(axis.text.x = element_text(angle = 45, hjust = 1))
