# R Script: Data Visualization for Patents Data
# ECON899 MA Paper 
# SFU Economics
# Daniel Sanchez
# Spring 2024 

# This script is used to prepare data visualizations of the patents data.

# Preliminaries -----------------------------------------------------------

# Load libraries

library(dplyr)
library(ggplot2)
library(scales)
library(patchwork)
library(lubridate, warn.conflicts = FALSE)

# Treatment start date

treatment_start_date <- ymd("2017-01-01")

# Valid start and end dates

start_date <- ymd("2001-01-01")

end_date <- ymd("2021-06-01")

# Load the data -----------------------------------------------------------

# Load the interested parties province-month panel data

interested_parties_province_month <- readRDS("data/patents/processed/interested_parties_province_month.rds")

patents_main <- readRDS("data/patents/processed/patents_main.rds")

# Load the final datasets (month and quarter)

df <- readRDS("data/full_dataset_monthly.rds")

df_quarterly <- readRDS("data/full_data_quarterly.rds")

# Time series by party type -----------------------------------------------------------

# Create a time series of the parties in patent applications by type in quarterly panel

df_quarterly %>%
    filter(quarter_year_date %>% between(start_date, end_date)) %>%
    select(quarter_year_date, ln1_interested_parties, ln1_inventors, ln1_applicants, ln1_owners) %>%
    pivot_longer(-quarter_year_date, names_to = "party_type", values_to = "ln1_parties") %>%
    filter(party_type == "ln1_inventors") %>%
    ggplot(aes(x = quarter_year_date, y = ln1_parties)) +
    geom_line() +
    facet_wrap(~party_type, scales = "free_y") +
    scale_x_date(date_labels = "%Y-%b") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))