# R Script: Statistics Canada Data Extraction
# ECON899 MA Paper 
# SFU Economics
# Daniel Sanchez
# Spring 2024 

# This script downloads and prepares variables from the Statistics Canada API for later processing with the rest of the data

# Preliminaries -----------------------------------------------------------

# Load packages

library(dplyr)
library(readr)
library(statcanR)
library(janitor)
library(tidyr)

# Load province codebook

provinces <- read_csv("data/other/province_codebook.csv", show_col_types = F)

# Labour Force Survey (LFS) -----------------------------------------------------------

# Extract and prepare variables from the Labour Force Survey (LFS)

## Labour force characteristics by province, monthly, seasonally adjusted -----------------------------------------------------------

# Extract the table using statcanR and clean names

lfs_lfc_prov_monthly <- 
    statcan_download_data("14-10-0287-03", "eng")  %>% 
    clean_names()

# Prepare working age population estimates (province-month). Consider total pop and also females males. Does not disaggregate by age group (15+ age group)

lfs_pop_province_monthly <- 
    lfs_lfc_prov_monthly  %>%
    filter(labour_force_characteristics == "Population",
           age_group == "15 years and over",
           data_type == "Seasonally adjusted",
           geo != "Canada",
           statistics == "Estimate")  %>% 
    select(month_year = ref_date, 
           geo,
           scale = scalar_factor, 
           value,
           sex)  %>%
    pivot_wider(names_from = sex,
                values_from = value,
                names_prefix = "pop")  %>% 
    clean_names() %>% 
    rename(total_pop = pop_both_sexes)  %>% 
    left_join(provinces %>% select(province, province_code), by = c("geo" = "province"))  %>%
    relocate(province_code, .after = geo)  %>%
    arrange(month_year, geo)

# Prepare employment estimates (province-month). Same demographics as before

lfs_emp_province_monthly <- 
    lfs_lfc_prov_monthly %>%
    filter(labour_force_characteristics == "Employment",
           age_group == "15 years and over",
           data_type == "Seasonally adjusted",
           geo != "Canada",
           statistics == "Estimate") %>% 
    select(month_year = ref_date, 
           geo,
           scale = scalar_factor,
           value,
           sex) %>%
    pivot_wider(names_from = sex,
                values_from = value,
                names_prefix = "emp") %>%
    clean_names() %>%
    rename(total_emp = emp_both_sexes) %>% 
    left_join(provinces %>% select(province, province_code), by = c("geo" = "province"))  %>%
    relocate(province_code, .after = geo) %>%
    arrange(month_year, geo)

# Prepare unemployment estimates (province-month). Same demographics as before

lfs_unem_province_monthly <- 
    lfs_lfc_prov_monthly %>%
    filter(labour_force_characteristics == "Unemployment",
           age_group == "15 years and over",
           data_type == "Seasonally adjusted",
           geo != "Canada",
           statistics == "Estimate") %>% 
    select(month_year = ref_date, 
           geo,
           scale = scalar_factor,
           value,
           sex) %>%
    pivot_wider(names_from = sex,
                values_from = value,
                names_prefix = "unem") %>%
    clean_names() %>%
    rename(total_unem = unem_both_sexes) %>% 
    left_join(provinces %>% select(province, province_code), by = c("geo" = "province"))  %>%
    relocate(province_code, .after = geo) %>%
    arrange(month_year, geo)

# Employment rate (province-month)

lfs_emp_rate_province_monthly <- 
    lfs_lfc_prov_monthly %>%
    filter(labour_force_characteristics == "Employment rate",
           age_group == "15 years and over",
           data_type == "Seasonally adjusted",
           geo != "Canada",
           statistics == "Estimate") %>% 
    select(month_year = ref_date, 
           geo,
           scale = scalar_factor,
           value,
           sex) %>%
    pivot_wider(names_from = sex,
                values_from = value,
                names_prefix = "emp_rate") %>%
    clean_names() %>%
    rename(total_emp_rate = emp_rate_both_sexes) %>% 
    left_join(provinces %>% select(province, province_code), by = c("geo" = "province"))  %>%
    relocate(province_code, .after = geo) %>%
    arrange(month_year, geo)

