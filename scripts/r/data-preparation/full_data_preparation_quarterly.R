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

# Define a treatment date (month-year in which agents were eligible to apply for the AITC)

treatment_start_date <- ymd("2017-01-01")

# Define a treatment quarter number (quarter the AITC was passed)

treatment_start_quarter <- quarter(treatment_start_date, type =  "year.quarter") %>% str_replace_all("\\.", "Q")

# Define the quarter date with floor_date

treatment_start_date_quarter <- floor_date(treatment_start_date, "quarter")

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
    mutate(quarter_year= quarter(month_year, type =  "year.quarter") %>% str_replace_all("\\.", "Q"),
            quarter_year_date = floor_date(month_year, "quarter")) %>%
    group_by(province_code, quarter_year, quarter_year_date) %>%
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
    monthly_explanatory %>%
    select(-contains("average"), -contains("median"), -contains("avg"), -cpi, -new_housing_price_index, -exp_index_econ_activity) %>% 
    mutate(quarter_year= quarter(month_year, type =  "year.quarter") %>% str_replace_all("\\.", "Q")) %>%
    group_by(province_code, quarter_year) %>%
    summarise(across(where(is.numeric),sum)) %>% 
    ungroup()

## Explanatory data, averages and medians -----------------------------------------------------------
# Take the value of the last month of the quarter

quarterly_explanatory_avg_med <-
    monthly_explanatory %>%
    select(month_year, province_code, contains("average"), contains("median"), contains("avg"), cpi, new_housing_price_index, exp_index_econ_activity) %>% 
    mutate(quarter_year= quarter(month_year, type =  "year.quarter") %>% str_replace_all("\\.", "Q")) %>%
    group_by(province_code, quarter_year) %>%
    summarise(across(where(is.numeric),last)) %>% 
    ungroup()

## Taking the logs -----------------------------------------------------------

# Patents, ln

quarterly_patents_ln <-
    quarterly_patents %>%
    mutate_at(vars(starts_with("patents_")), ~log(.)) %>%
    rename_with(~paste0("ln_", .), starts_with("patents_")) %>%
    select(-foreign_parties, -quarter_year_date)

# Patents, ln +1 

quarterly_patents_ln1 <-
    quarterly_patents %>%
    mutate_at(vars(starts_with("patents_")), ~log(. + 1))  %>%
    rename_with(~paste0("ln1_", .), starts_with("patents_")) %>%
    select(-foreign_parties, -quarter_year_date)

# Interested parties, ln

quarterly_parties_ln <-
    quarterly_parties %>%
    mutate_at(vars(starts_with("interested_parties")), ~log(.))  %>%
    rename_with(~paste0("ln_", .), starts_with(c("interested_parties", "owners", "inventors", "applicants")))

# Interested parties, ln +1

quarterly_parties_ln1 <-
    quarterly_parties %>%
    mutate_at(vars(starts_with("interested_parties")), ~log(. + 1))  %>%
    rename_with(~paste0("ln1_", .), starts_with(c("interested_parties", "owners", "inventors", "applicants")))

# Stock explanatory variables, ln

quarterly_explanatory_stock_ln <-
    quarterly_explanatory_stock %>%
    mutate_at(vars(-province_code, -quarter_year), ~log(.)) %>%
    rename_if(is.numeric, ~paste0("ln_",.))

# Stock explantory variables, ln + 1

quarterly_explanatory_stock_ln1 <-
    quarterly_explanatory_stock %>%
    mutate_at(vars(-province_code, -quarter_year), ~log(. + 1)) %>%
    rename_if(is.numeric, ~paste0("ln1_",.))

# Averages and medians explanatory variables, ln

quarterly_explanatory_avg_med_ln <-
    quarterly_explanatory_avg_med %>%
    select(-cpi, -new_housing_price_index, -exp_index_econ_activity) %>% 
    mutate_at(vars(-province_code, -quarter_year,), ~log(.)) %>%
    rename_if(is.numeric, ~paste0("ln_",.))

# Averages and medians explanatory variables, ln + 1

quarterly_explanatory_avg_med_ln1 <-
    quarterly_explanatory_avg_med %>%
    select(-cpi, -new_housing_price_index, -exp_index_econ_activity) %>% 
    mutate_at(vars(-province_code, -quarter_year), ~log(. + 1)) %>%
    rename_if(is.numeric, ~paste0("ln1_",.))

# Final dataset -----------------------------------------------------------

# Join all dataframes together

df <- 
    quarterly_patents %>%
    filter(!(province_code %in% c("YT", "NT", "NU", "PE", "NL"))) %>% 
    left_join(quarterly_patents_ln, by = c("province_code", "quarter_year")) %>%
    left_join(quarterly_patents_ln1, by = c("province_code", "quarter_year")) %>%
    left_join(quarterly_parties, by = c("province_code" = "province_code_clean", "quarter_year")) %>%
    left_join(quarterly_parties_ln, by = c("province_code" = "province_code_clean", "quarter_year")) %>%
    left_join(quarterly_parties_ln1, by = c("province_code" = "province_code_clean", "quarter_year"))  %>% 
    left_join(quarterly_explanatory_stock, by = c("province_code", "quarter_year")) %>% 
    left_join(quarterly_explanatory_stock_ln, by = c("province_code", "quarter_year")) %>% 
    left_join(quarterly_explanatory_stock_ln1, by = c("province_code", "quarter_year")) %>%
    left_join(quarterly_explanatory_avg_med, by = c("province_code", "quarter_year")) %>%
    left_join(quarterly_explanatory_avg_med_ln, by = c("province_code", "quarter_year")) %>%
    left_join(quarterly_explanatory_avg_med_ln1, by = c("province_code", "quarter_year")) %>% 
    mutate(province_code = as_factor(province_code),
           periods = interval(treatment_start_date_quarter, quarter_year_date)/months(3),
           treatment = if_else(province_code == treatment_group, "Treatment", "Control") %>% as.factor() %>% relevel("Control"),
           post = if_else(quarter_year >= treatment_start_quarter, "Post", "Pre") %>% as.factor() %>% relevel("Pre"),
           ln1_foreign_parties = log(foreign_parties+1)) %>% 
    arrange(province_code, quarter_year)

# Exporting the data -----------------------------------------------------------

# Export the full dataset to an RDS file

saveRDS(df, "data/full_data_quarterly.rds")

# Export the full dataset to a CSV file (for other software)

write_csv(df, "data/full_data_quarterly.csv")