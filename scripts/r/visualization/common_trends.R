# R Script: Common trends data visualization
# ECON899 MA Paper 
# SFU Economics
# Daniel Sanchez
# Spring 2024 

# This script implements data visualization for the common trends assumption in the difference-in-differences models.

# Preliminaries -----------------------------------------------------------

# Load libraries

library(dplyr, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
library(scales, warn.conflicts = FALSE)
library(patchwork, warn.conflicts = )
library(lubridate, warn.conflicts = FALSE)
library(stringr, warn.conflicts = FALSE)

# Loading data ------------------------------------------------------------

# Load the final datasets

df_monthly <- readRDS("data/full_dataset_monthly.rds")

df_quarterly <- readRDS("data/full_data_quarterly.rds")

# Dates

# Define treatment start date

treatment_start_date <- ymd("2017-01-01")

# Define valid start and end dates

start_date <- ymd("2001-01-01")

end_date <- ymd("2021-06-01")

# Get quarter dates with floor_date

start_date_quarter <- floor_date(start_date, "quarter")

end_date_quarter <- floor_date(end_date, "quarter")

treatment_start_date <- ymd("2017-01-01")

treatment_start_date_quarter <- floor_date(treatment_start_date, "quarter")

# Same but with the quarter formula

start_quarter_q <- quarter(start_date, type =  "year.quarter") %>% str_replace_all("\\.", "Q")

end_quarter_q <- quarter(end_date, type =  "year.quarter") %>% str_replace_all("\\.", "Q")

treatment_start_quarter_q <- quarter(treatment_start_date, type =  "year.quarter") %>% str_replace_all("\\.", "Q")

# Define valid start and end periods

start_period <- interval(start_date_quarter, treatment_start_date_quarter)/months(3)

end_period <- interval(treatment_start_date_quarter, end_date_quarter)/months(3)

# Define df (relevant data) within the valid start period and end period already defined

df <- 
    df_quarterly %>%
    filter(quarter_year_date %>% between(start_date_quarter, end_date_quarter))

labels <-
    df %>% 
    select(quarter_year) %>% 
    distinct() %>%
    pull()

# Patents by province and quarter common trends --------------------------------

# Plotting the common trends for patents by province and quarter

ln_applications_by_quarter <- 
    df %>%
    group_by(periods, quarter_year, quarter_year_date, treatment) %>% 
    summarise(patents = sum(patents_filed)) %>%
    mutate(ln1_patents_filed = log(patents + 1)) %>%
    ungroup() 

periods_break <- seq(min(ln_applications_by_quarter$periods), max(ln_applications_by_quarter$periods), by = 4)

periods_labels <-
    df %>% 
    filter(periods %in% periods_break) %>%
    select(quarter_year) %>%
    pull() %>% 
    unique()

periods_min <- min(ln_applications_by_quarter$periods)

periods_max <- max(ln_applications_by_quarter$periods)

quarterly_common_trends <-
    ln_applications_by_quarter %>% 
    ggplot(aes(x = periods, y = ln1_patents_filed, group = treatment, colour = treatment)) +
    geom_line() + 
    scale_y_continuous(labels = seq(1, 10, by = 1), breaks = seq(1, 10, by = 1)) +
    scale_x_continuous(breaks = periods_break, labels = periods_labels, limits = c(periods_min, periods_max)) +
    scale_color_manual(values = c("Treatment" = "#0D3692", "Control" = "#E60F2D")) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
    labs(colour = "",
         x = "",
         y = "Ln (Patent applications + 1)")+
    theme_minimal() +
    theme(text = element_text(size = 10),
          axis.ticks.x = element_line(colour = "gray50"),
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.line.x = element_line(colour = "black"),
          plot.background = element_rect(fill = "white", color = "white"),
          panel.border = element_rect(colour = "black", fill = NA, linewidth = 1, linetype = "solid"),
          plot.caption = element_text(hjust = 0),
          panel.grid.major = element_line(linetype = "dashed"),
          panel.grid.minor = element_line(linetype = "dashed"),
          legend.position = c(0.9, 0.15))

quarterly_common_trends

# Save the plot

ggsave("figures/quarterly_common_trends.png", 
        quarterly_common_trends, 
        width = 15, 
        height = 10,
        units = "cm", 
        dpi = 800)