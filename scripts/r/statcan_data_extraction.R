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

# Employment, both part-time and full-time

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

# Full-time employment

lfs_full_emp_province_monthly <- 
    lfs_lfc_prov_monthly %>%
    filter(labour_force_characteristics == "Full-time employment",
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
                names_prefix = "full_emp") %>%
    clean_names() %>%
    rename(total_full_emp = full_emp_both_sexes) %>% 
    left_join(provinces %>% select(province, province_code), by = c("geo" = "province"))  %>%
    relocate(province_code, .after = geo) %>%
    arrange(month_year, geo)

# Part-time employment

lfs_part_emp_province_monthly <- 
    lfs_lfc_prov_monthly %>%
    filter(labour_force_characteristics == "Part-time employment ",
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
                names_prefix = "part_emp") %>%
    clean_names() %>%
    rename(total_part_emp = part_emp_both_sexes) %>% 
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

## Employee wages -----------------------------------------------------------

# Extract the table using statcanR and clean names 

lfs_wages_industry_prov_monthly <- 
    statcan_download_data("14-10-0063-01", "eng")  %>% 
    clean_names()

# Total wages paid, province-month

lfs_total_wages_prov <- 
    lfs_wages_industry_prov_monthly %>%
    filter(wages == "Total employees, all wages",
           type_of_work == "Both full- and part-time employees",
           age_group == "15 years and over",
           north_american_industry_classification_system_naics == "Total employees, all industries",
           geo != "Canada") %>%
    select(month_year = ref_date, 
           sex,
           geo,
           scale = scalar_factor,
           value) %>%
    pivot_wider(names_from = sex,
                values_from = value,
                names_prefix = "total_wage") %>%
    clean_names() %>%
    rename(total_wage = total_wage_both_sexes) %>%
    left_join(provinces %>% select(province, province_code), by = c("geo" = "province"))  %>%
    relocate(province_code, .after = geo) %>%
    arrange(month_year, geo)

# Average hourly wage, province-month

lfs_average_hourly_wage <- 
    lfs_wages_industry_prov_monthly %>%
    filter(wages == "Average hourly wage rate",
           type_of_work == "Both full- and part-time employees",
           age_group == "15 years and over",
           north_american_industry_classification_system_naics == "Total employees, all industries",
           geo != "Canada") %>%
    select(month_year = ref_date, 
           sex,
           geo,
           scale = scalar_factor,
           value) %>%
    pivot_wider(names_from = sex,
                values_from = value,
                names_prefix = "avg_wage") %>%
    clean_names() %>%
    rename(total_avg_wage = avg_wage_both_sexes) %>%
    left_join(provinces %>% select(province, province_code), by = c("geo" = "province"))  %>%
    relocate(province_code, .after = geo) %>%
    arrange(month_year, geo)

# Median hourly wage, province-month

lfs_median_hourly_wage <- 
    lfs_wages_industry_prov_monthly %>%
    filter(wages == "Median hourly wage rate",
           type_of_work == "Both full- and part-time employees",
           age_group == "15 years and over",
           north_american_industry_classification_system_naics == "Total employees, all industries",
           geo != "Canada") %>%
    select(month_year = ref_date, 
           sex,
           geo,
           scale = scalar_factor,
           value) %>%
    pivot_wider(names_from = sex,
                values_from = value,
                names_prefix = "median_wage") %>%
    clean_names() %>%
    rename(total_median_wage = median_wage_both_sexes) %>%
    left_join(provinces %>% select(province, province_code), by = c("geo" = "province"))  %>%
    relocate(province_code, .after = geo) %>%
    arrange(month_year, geo)

## Usual hours worked -----------------------------------------------------------

# Extract the table for usual hours worked using statcanR and clean names

lfs_usual_hours_worked_prov_monthly <- 
    statcan_download_data("14-10-0030-01", "eng")  %>% 
    clean_names()

# Total hours worked, province-month

lfs_usual_total_hours_worked_prov <- 
    lfs_usual_hours_worked_prov_monthly %>%
    filter(geo != "Canada",
           usual_hours_worked == "Total usual hours",
           age_group == "15 years and over",
           job == "All jobs") %>% 
    select(month_year = ref_date, 
           sex,
           geo,
           scale = scalar_factor,
           value) %>% 
    pivot_wider(names_from = sex,
                values_from = value,
                names_prefix = "total_hours") %>%
    clean_names() %>%
    rename(total_hours = total_hours_both_sexes) %>%
    left_join(provinces %>% select(province, province_code), by = c("geo" = "province"))  %>%
    relocate(province_code, .after = geo) %>%
    arrange(month_year, geo)

# Average hours worked, province-month

lfs_usual_total_hours_worked_prov <- 
    lfs_usual_hours_worked_prov_monthly %>%
    filter(geo != "Canada",
           usual_hours_worked == "Average usual hours",
           age_group == "15 years and over",
           job == "All jobs") %>% 
    select(month_year = ref_date, 
           sex,
           geo,
           scale = scalar_factor,
           value) %>% 
    pivot_wider(names_from = sex,
                values_from = value,
                names_prefix = "average_hours") %>%
    clean_names() %>%
    rename(total_average_hours = "average_hours_both_sexes") %>%
    left_join(provinces %>% select(province, province_code), by = c("geo" = "province"))  %>%
    relocate(province_code, .after = geo) %>%
    arrange(month_year, geo)

# Employment Insurance (EI) -----------------------------------------------------------
