# R Script: Full Dataset Preparation - Quarterly
# ECON899 MA Paper 
# SFU Economics
# Daniel Sanchez
# Spring 2024 

# This script prepares the full dataset for the analysis of the ECON899 MA Paper, at the quarterly frequency. 

# Preliminaries -----------------------------------------------------------

# Load packages

library(dplyr, warn.conflicts = F)
library(readr, warn.conflicts = F)
library(forcats, warn.conflicts = F)
library(lubridate, warn.conflicts = F)
library(tidyr, warn.conflicts = F)
library(janitor, warn.conflicts = F)
library(stringr, warn.conflicts = F)

# Define a treatment date (month-year the AITC was passed)

treatment_start_date <- ymd("2016-08-01")

# Define a treatment quarter number (quarter the AITC was passed)

treatment_start_quarter <- quarter(treatment_start_date, type =  "year.quarter") %>% str_replace_all("\\.", "Q")

# Define the quarter date with floor_date

treatment_start_quarter_date <- floor_date(treatment_start_date, "quarter")

# Define a treatment period (Alberta)

treatment_group <- "AB"

# Load the data -----------------------------------------------------------

# Load monthly patents data

monthly_patents <- readRDS("data/patents/processed/patents_per_province.rds")

# Load monthly parties data

monthly_parties <- readRDS("data/patents/processed/interested_parties_province_month.rds")

# Quarterly data preparation

# Patents

quarterly_patents <-
    monthly_patents %>% 
    mutate(quarter_year= quarter(month_year, type =  "year.quarter") %>% str_replace_all("\\.", "Q")) %>%
    group_by(province_code, quarter_year) %>%
    summarise(across(where(is.integer),sum)) %>% 
    ungroup()

# Interested parties

quarterly_parties <-
    monthly_parties %>% 
    mutate(quarter_year= quarter(filing_month_year, type =  "year.quarter") %>% str_replace_all("\\.", "Q")) %>%
    group_by(province_code_clean, quarter_year) %>%
    summarise(across(where(is.integer),sum)) %>% 
    ungroup()