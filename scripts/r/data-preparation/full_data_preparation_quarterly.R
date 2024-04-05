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

# Load monthly explanatory variables data

monthly_explanatory <- 
       read_csv("data/explanatory_vars_province_month_panel.csv", show_col_types = F) %>%
       mutate(electric_power_generation = if_else(electric_power_generation < 0, 0, electric_power_generation)) 

# Quarterly data preparation -----------------------------------------------------------

## Patents -----------------------------------------------------------

quarterly_patents <-
    monthly_patents %>% 
    mutate(quarter_year= quarter(month_year, type =  "year.quarter") %>% str_replace_all("\\.", "Q")) %>%
    group_by(province_code, quarter_year) %>%
    summarise(across(where(is.integer),sum)) %>% 
    ungroup()

## Interested parties -----------------------------------------------------------

quarterly_parties <-
    monthly_parties %>% 
    mutate(quarter_year= quarter(filing_month_year, type =  "year.quarter") %>% str_replace_all("\\.", "Q")) %>%
    group_by(province_code_clean, quarter_year) %>%
    summarise(across(where(is.integer),sum)) %>% 
    ungroup()

## Explanatory data, stock variables (employment, population, etc.) -----------------------------------------------------------

# Sum all the values of the month in the quarter

quarterly_explanatory_stock <-
    monthly_statcan %>%
    select(-contains("average"), -contains("median"), -contains("avg"), -cpi, -new_housing_price_index) %>% 
    mutate(quarter_year= quarter(month_year, type =  "year.quarter") %>% str_replace_all("\\.", "Q")) %>%
    group_by(province_code, quarter_year) %>%
    summarise(across(where(is.numeric),sum)) %>% 
    ungroup()

## Explanatory data, averages and medians -----------------------------------------------------------
# Take the value of the last month of the quarter

quarterly_explanatory_avg_med <-
    monthly_statcan %>%
    select(month_year, province_code, contains("average"), contains("median"), contains("avg"), cpi, new_housing_price_index, new_housing_price_index) %>% 
    mutate(quarter_year= quarter(month_year, type =  "year.quarter") %>% str_replace_all("\\.", "Q")) %>%
    group_by(province_code, quarter_year) %>%
    summarise(across(where(is.numeric),last)) %>% 
    ungroup()

## Taking the logs -----------------------------------------------------------

quarterly_patents_ln1 <-
    quarterly_patents %>%
    mutate_at(vars(starts_with("patents_")), ~log(. + 1))  %>%
    rename_with(~paste0("ln1_", .), starts_with("patents_")) %>%
    select(-foreign_parties)

quarterly_patents_ln <-
    quarterly_patents %>%
    mutate_at(vars(starts_with("patents_")), ~log(.)) %>%
    rename_with(~paste0("ln_", .), starts_with("patents_")) %>%
    select(-foreign_parties)

quarterly_parties_ln1 <-
    quarterly_parties %>%
    mutate_at(vars(starts_with("interested_parties")), ~log(. + 1))  %>%
    rename_with(~paste0("ln1_", .), starts_with("interested_parties"))

quarterly_parties_ln <-
    quarterly_parties %>%
    mutate_at(vars(starts_with("interested_parties")), ~log(.))  %>%
    rename_with(~paste0("ln_", .), starts_with("interested_parties"))

quarterly_explanatory_stock_ln <-
    quarterly_statcan_stock %>%
    mutate_at(vars(-cpi), ~log(. + 1))  %>%
    rename_with(~paste0("ln1_", .), -cpi)