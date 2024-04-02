# R Script: Statistics Canada Data Extraction
# ECON899 MA Paper 
# SFU Economics
# Daniel Sanchez
# Spring 2024 

# This script downloads data from the Statistics Canada API for later processing 

# Preliminaries -----------------------------------------------------------

# Load packages

library(dplyr)
library(statcanR)
library(janitor)
library(readr)

# Extract data from Statistics Canada -----------------------------------------------------------

# Use statcanR to extract data from the Statistics Canada API.
# Load all raw data tables at once, I will process data later
# Only use janitor's clean_names to clean the names of the columns for easier loading

## Labour Force Survey (LFS) -----------------------------------------------------------

# Labour force characteristics by province, monthly, seasonally adjusted

lfs_lfc_prov_monthly <- 
    statcan_download_data("14-10-0287-03", "eng")  %>% 
    clean_names()

# Employment by industry, monthly, seasonally unadjusted

lfs_employment_industry_monthly <- 
    statcan_download_data("14-10-0355-01", "eng")  %>% 
    clean_names()

# Employee wages by industry 

lfs_wages_industry_prov_monthly <- 
    statcan_download_data("14-10-0063-01", "eng")  %>% 
    clean_names()

# Usual hours worked by province

lfs_usual_hours_worked_prov_monthly <- 
    statcan_download_data("14-10-0030-01", "eng")  %>% 
    clean_names()

# Actual hours worked by province

lfs_actual_hours_worked_prov_monthly <- 
    statcan_download_data("14-10-0032-01", "eng")  %>% 
    clean_names()

# Job tenure by type of work (full- and part-time), monthly, unadjusted for seasonality

lfs_job_tenure_type_work_monthly <- 
    statcan_download_data("14-10-0050-01", "eng")  %>% 
    clean_names()

# Employment Insurance (EI) claims -----------------------------------------------------------

# EI Claims by province, monthly, seasonally adjusted

ei_claims_prov_monthly_table <- 
    statcan_download_data("14-10-0005-01", "eng")  %>% 
    clean_names()

# CPI by province, monthly, not seasonally adjusted ------------------------------------------

cpi_prov_monthly_table <- 
    statcan_download_data("18-10-0004-01", "eng")  %>% 
    clean_names()

# Sales & revenue data -----------------------------------------------------------

# Retail trade sales by province, monthly, seasonally adjusted

retail_trade_sales_prov_monthly_table <- 
    statcan_download_data("20-10-0008-01", "eng")  %>% 
    clean_names()

# Wholesale trade sales by province, monthly, seasonally adjusted

wholesale_trade_sales_prov_monthly_table <- 
    statcan_download_data("20-10-0074-01", "eng")  %>% 
    clean_names()

# Manufacturing sales by province, monthly, seasonally adjusted

manufacturing_sales_prov_monthly_table <- 
    statcan_download_data("16-10-0048-01", "eng")  %>% 
    clean_names()

# Monthly survey of food services and drinking places by province, monthly, seasonally adjusted

food_services_sales_prov_monthly_table <- 
    statcan_download_data("21-10-0019-01", "eng")  %>% 
    clean_names()

# Mobility -----------------------------------------------------------

# Vehicles entering canada 

vehicles_entering_canada_monthly_table <- 
    statcan_download_data("24-10-0052-01", "eng")  %>% 
    clean_names()

# International travellers entering Canada by province, monthly, seasonally adjusted

international_travellers_canada_monthly_table <- 
    statcan_download_data("24-10-0005-01", "eng") %>% 
    clean_names()

# Electric power generation by province, monthly, seasonally adjusted

# First table from 1950 to 2007

electric_power_generation_prov_monthly_table_1 <- 
    statcan_download_data("25-10-0001-01", "eng") %>% 
    clean_names()

# Second table from 2008 to 2021

electric_power_generation_prov_monthly_table_2 <- 
    statcan_download_data("25-10-0015-01", "eng") %>% 
    clean_names()

# Experimental indexes of economic activity by province, monthly, seasonally adjusted

experimental_econ_activity_prov_monthly_table <- 
    statcan_download_data("36-10-0633-01", "eng")  %>% 
    clean_names()

# International trade -----------------------------------------------------------

# International merchandise trade 

international_merchandise_trade_monthly_table <- 
    statcan_download_data("12-10-0175-01", "eng")  %>% 
    clean_names()

# Housing -----------------------------------------------------------

# New housing price index

new_housing_price_index_monthly_table <- 
    statcan_download_data("18-10-0205-01", "eng") %>% 
    clean_names()

# Building permits 1 1978 - 2017

building_permits_monthly_table_1 <- 
    statcan_download_data("34-10-0002-01", "eng") %>% 
    clean_names()

# Bulding permits 2017 - 2024

building_permits_monthly_table_2 <- 
    statcan_download_data("34-10-0285-01", "eng") %>% 
    clean_names()

# Export data to csv files -----------------------------------------------------------

# Export the data to csv files for later processing ------------------------------------

write_csv(lfs_lfc_prov_monthly, "data/statcan/raw/lfs_lfc_prov_monthly.csv")

write_csv(lfs_employment_industry_monthly, "data/statcan/raw/lfs_employment_industry_monthly.csv")

write_csv(lfs_wages_industry_prov_monthly, "data/statcan/raw/lfs_wages_industry_prov_monthly.csv")

write_csv(lfs_usual_hours_worked_prov_monthly, "data/statcan/raw/lfs_usual_hours_worked_prov_monthly.csv")

write_csv(lfs_actual_hours_worked_prov_monthly, "data/statcan/raw/lfs_actual_hours_worked_prov_monthly.csv")

write_csv(lfs_job_tenure_type_work_monthly, "data/statcan/raw/lfs_job_tenure_type_work_monthly.csv")

write_csv(ei_claims_prov_monthly_table, "data/statcan/raw/ei_claims_prov_monthly_table.csv")

write_csv(cpi_prov_monthly_table, "data/statcan/raw/cpi_prov_monthly_table.csv")

write_csv(retail_trade_sales_prov_monthly_table, "data/statcan/raw/retail_trade_sales_prov_monthly_table.csv")

write_csv(wholesale_trade_sales_prov_monthly_table, "data/statcan/raw/wholesale_trade_sales_prov_monthly_table.csv")

write_csv(manufacturing_sales_prov_monthly_table, "data/statcan/raw/manufacturing_sales_prov_monthly_table.csv")

write_csv(food_services_sales_prov_monthly_table, "data/statcan/raw/food_services_sales_prov_monthly_table.csv")

write_csv(vehicles_entering_canada_monthly_table, "data/statcan/raw/vehicles_entering_canada_monthly_table.csv")

write_csv(international_travellers_canada_monthly_table, "data/statcan/raw/international_travellers_canada_monthly_table.csv")

write_csv(electric_power_generation_prov_monthly_table_1, "data/statcan/raw/electric_power_generation_prov_monthly_table_1.csv")

write_csv(electric_power_generation_prov_monthly_table_2, "data/statcan/raw/electric_power_generation_prov_monthly_table_2.csv")

write_csv(experimental_econ_activity_prov_monthly_table, "data/statcan/raw/experimental_econ_activity_prov_monthly_table.csv")

write_csv(international_merchandise_trade_monthly_table, "data/statcan/raw/international_merchandise_trade_monthly_table.csv")

write_csv(new_housing_price_index_monthly_table, "data/statcan/raw/new_housing_price_index_monthly_table.csv")

write_csv(building_permits_monthly_table_1, "data/statcan/raw/building_permits_monthly_table_1.csv")

write_csv(building_permits_monthly_table_2, "data/statcan/raw/building_permits_monthly_table_2.csv")

