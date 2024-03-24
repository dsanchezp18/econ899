# R Script: Explanatory Variable Data Preparation
# ECON899 MA Paper 
# SFU Economics
# Daniel Sanchez
# Spring 2024 

# This script loads the downloaded data and prepares it for analysis. It also exports the data to a csv file for later joining with the rest of the data

# Preliminaries -----------------------------------------------------------

# Load packages

library(readr, warn.conflicts = F)
library(statcanR, warn.conflicts = F)
library(janitor, warn.conflicts = F)
library(tidyr, warn.conflicts = F)
library(dplyr, warn.conflicts = F)
library(stringr, warn.conflicts = F)
library(readxl, warn.conflicts = F)
library(lubridate, warn.conflicts = F)

# Load province codebook

provinces <- read_csv("data/other/province_codebook.csv", 
                       show_col_types = F)

# Load raw data tables -----------------------------------------------------------

## StatCan data -----------------------------------------------------------

# List the raw files in the StatCan data folder (with full path)

statcan_raw_data_files <- list.files("data/statcan/raw", recursive = T, full.names = T)

# Use fread and lapply to load all raw data tables at once with lapply

statcan_raw_data <- lapply(statcan_raw_data_files, data.table::fread)

# Convert all to tibbles and turn all ref_date columns to date formats with lubridate

statcan_raw_data <- lapply(statcan_raw_data, as_tibble) %>% 
    lapply(function(x) {
        x %>% 
            mutate(ref_date = ymd(ref_date))
    })

# Load the file names without the full path

statcan_object_names <- list.files("data/statcan/raw", recursive = T)

# Pass the object names to the list of dataframes, having removed the .csv extension with stringr

names(statcan_raw_data) <- str_remove(statcan_object_names, ".csv")

# Assign the dataframes to the global environment

list2env(statcan_raw_data, envir = .GlobalEnv)

# Data preparation for StatCan data -----------------------------------------------------------

## Labour Force Survey (LFS) -----------------------------------------------------------

# Extract and prepare variables from the Labour Force Survey (LFS)

### Labour force characteristics by province, monthly, seasonally adjusted -----------------------------------------------------------

# Prepare working age population estimates (province-month). Consider total pop and also females males. Does not disaggregate by age group (15+ age group)

lfs_pop_province_monthly <- 
    lfs_lfc_prov_monthly %>%
    filter(labour_force_characteristics == "Population",
           age_group == "15 years and over",
           data_type == "Seasonally adjusted",
           geo != "Canada",
           statistics == "Estimate") %>% 
    select(month_year = ref_date, 
           geo,
           scale = scalar_factor, 
           value,
           sex)  %>%
    pivot_wider(names_from = sex,
                values_from = value,
                names_prefix = "pop") %>% 
    clean_names() %>% 
    rename(total_pop = pop_both_sexes) %>% 
    left_join(provinces %>% select(province, province_code), by = c("geo" = "province")) %>%
    relocate(province_code, .after = geo) %>%
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
    left_join(provinces %>% select(province, province_code), by = c("geo" = "province")) %>%
    relocate(province_code, .after = geo) %>%
    arrange(month_year, geo)

# Part-time employment

lfs_part_emp_province_monthly <-
    lfs_lfc_prov_monthly %>%
    filter(labour_force_characteristics == "Part-time employment",
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

### Employee wages -----------------------------------------------------------

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
    left_join(provinces %>% select(province, province_code), by = c("geo" = "province")) %>%
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
    left_join(provinces %>% select(province, province_code), by = c("geo" = "province")) %>%
    relocate(province_code, .after = geo) %>%
    arrange(month_year, geo)

### Usual hours worked -----------------------------------------------------------

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
                names_prefix = "total_usual_hours") %>%
    clean_names() %>%
    rename(total_usual_hours = total_usual_hours_both_sexes) %>%
    left_join(provinces %>% select(province, province_code), by = c("geo" = "province")) %>%
    relocate(province_code, .after = geo) %>%
    arrange(month_year, geo)

# Average hours worked, province-month

lfs_usual_avg_hours_worked_prov <- 
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
                names_prefix = "average_usual_hours") %>%
    clean_names() %>%
    rename(average_usual_hours = "average_usual_hours_both_sexes") %>%
    left_join(provinces %>% select(province, province_code), by = c("geo" = "province"))  %>%
    relocate(province_code, .after = geo) %>%
    arrange(month_year, geo)

### Actual hours worked -----------------------------------------------------------

# Total hours worked, province-month

lfs_actual_total_hours_worked_prov <- 
    lfs_actual_hours_worked_prov_monthly %>%
    filter(geo != "Canada",
           actual_hours_worked == "Total actual hours",
           age_group == "15 years and over",
           job == "All jobs") %>%
    select(month_year = ref_date,
           sex,
           geo,
           scale = scalar_factor,
           value) %>%
    pivot_wider(names_from = sex,
                values_from = value,
                names_prefix = "total_actual_hours") %>%
    clean_names() %>%
    rename(total_actual_hours = "total_actual_hours_both_sexes") %>% 
    left_join(provinces %>% select(province, province_code), by = c("geo" = "province"))  %>%
    relocate(province_code, .after = geo) %>%
    arrange(month_year, geo)

# Average hours worked, province-month

lfs_actual_avg_hours_worked_prov <- 
    lfs_actual_hours_worked_prov_monthly %>%
    filter(geo != "Canada",
           actual_hours_worked == "Average actual hours (all workers)",
           age_group == "15 years and over",
           job == "All jobs") %>%
    select(month_year = ref_date,
           sex,
           geo,
           scale = scalar_factor,
           value) %>%
       pivot_wider(names_from = sex,
                   values_from = value,
                   names_prefix = "average_actual_hours") %>%
       clean_names() %>%
       rename(average_actual_hours = "average_actual_hours_both_sexes") %>%
       left_join(provinces %>% select(province, province_code), by = c("geo" = "province")) %>%
       relocate(province_code, .after = geo) %>%
       arrange(month_year, geo)

### Job tenure by province-month -----------------------------------------------------------

# Average job tenure by type of work (full- and part-time), monthly, unadjusted for seasonality

lfs_avg_job_tenure_prov_monthly <-
    lfs_job_tenure_type_work_monthly %>%
    filter(geo != "Canada",
           job_tenure == "Average tenure",
           age_group == "15 years and over",
           type_of_work == "Both full and part-time employment") %>%
    select(month_year = ref_date,
           sex,
           geo,
           scale = scalar_factor,
           value) %>%
    pivot_wider(names_from = sex,
                values_from = value,
                names_prefix = "avg_tenure") %>%
    clean_names() %>% 
    rename(total_avg_tenure = "avg_tenure_both_sexes") %>% 
    left_join(provinces %>% select(province, province_code), by = c("geo" = "province")) %>%
    relocate(province_code, .after = geo) %>%
    arrange(month_year, geo)

## Employment Insurance (EI) -----------------------------------------------------------

### Claims by province, monthly, seasonally adjusted -----------------------------------------------------------

# Get month-province claims 

ei_claims_prov_monthly <-
    ei_claims_prov_monthly_table %>%
    filter(geo != "Canada",
           type_of_claim == "Initial and renewal claims, seasonally adjusted",
           claim_detail == "Received",
           uom == "Claims", ) %>%
    select(month_year = ref_date,
           geo,
           scale = scalar_factor,
           ei_claims = value) %>%
    clean_names() %>%
    left_join(provinces %>% select(province, province_code), by = c("geo" = "province")) %>%
    relocate(province_code, .after = geo) %>%
    arrange(month_year, geo)

## Consumer Price Index (CPI) -----------------------------------------------------------

### CPI, by province, monthly, not seasonally adjusted -----------------------------------------------------------
   
# All items, province-month (eliminating CMAs)

cpi_all_items_prov_monthly <-
    cpi_prov_monthly_table %>%
    filter(geo != "Canada",
           products_and_product_groups == "All-items") %>%
    select(month_year = ref_date,
           base_year = uom,
           geo,
           scale = scalar_factor,
           cpi = value) %>%
    clean_names() %>%
    left_join(provinces %>% select(province, province_code), by = c("geo" = "province")) %>%
    relocate(province_code, .after = geo) %>%
    filter(!is.na(province_code)) %>% 
    arrange(month_year, geo)

## Industry sales -----------------------------------------------------------

### Retrail trade sales -----------------------------------------------------------

# Retail trade sales, province-month panel 

retail_trade_sales_prov_monthly <-
       retail_trade_sales_prov_monthly_table %>%
       filter(geo != "Canada",
              north_american_industry_classification_system_naics == "Retail trade [44-45]",
              adjustments == "Seasonally adjusted") %>%
       select(month_year = ref_date, 
              geo,
              scale = scalar_factor,
              retail_sales = value) %>%
       clean_names() %>%
       left_join(provinces %>% select(province, province_code), by = c("geo" = "province")) %>%
       relocate(province_code, .after = geo) %>%
       arrange(month_year, geo)

### Wholesale trade sales -----------------------------------------------------------

# Wholesale trade sales, province-month panel

wholesale_trade_sales_prov_monthly <-
       wholesale_trade_sales_prov_monthly_table %>%
       filter(geo != "Canada",
              north_american_industry_classification_system_naics == "Wholesale trade [41]",
              adjustments == "Seasonally adjusted") %>%
       select(month_year = ref_date,
              geo,
              scale = scalar_factor,
              wholesale_sales = value) %>%
       clean_names() %>%
       left_join(provinces %>% select(province, province_code), by = c("geo" = "province")) %>%
       relocate(province_code, .after = geo) %>%
       arrange(month_year, geo)

### Manufacturing sales -----------------------------------------------------------

# Manufacturing sales, province-month panel

manufacturing_sales_prov_monthly <-
       manufacturing_sales_prov_monthly_table %>%
       filter(geo != "Canada",
              north_american_industry_classification_system_naics == "Manufacturing [31-33]",
              seasonal_adjustment == "Seasonally adjusted") %>%
       select(month_year = ref_date, 
              geo,
              scale = scalar_factor,
              manufacturing_sales = value) %>%
       clean_names() %>%
       left_join(provinces %>% select(province, province_code), by = c("geo" = "province")) %>%
       relocate(province_code, .after = geo) %>%
       arrange(month_year, geo)

### Monthly survey of food services and drinking places -----------------------------------------------------------

# Get sales (receipts) from food services and drinking places, province-month panel

food_services_sales_prov_monthly <-
       food_services_sales_prov_monthly_table %>%
       filter(geo != "Canada",
              north_american_industry_classification_system_naics == "Total, food services and drinking places",
              seasonal_adjustment == "Seasonally adjusted",
              service_detail == "Receipts") %>%
       select(month_year = ref_date, 
              geo,
              scale = scalar_factor,
              food_services_receipts = value) %>%
       clean_names() %>%
       left_join(provinces %>% select(province, province_code), by = c("geo" = "province")) %>%
       relocate(province_code, .after = geo) %>%
       arrange(month_year, geo)

## Mobility data -----------------------------------------------------------

### International travellers entering Canada -----------------------------------------------------------

# Get international travellers entering Canada, province-month panel

international_travellers_canada_monthly <-
       international_travellers_canada_monthly_table %>%
       filter(geo != "Canada",
              traveller_category == "Total non resident travellers") %>%
       select(month_year = ref_date, 
              geo,
              scale = scalar_factor,
              travellers = value) %>%
       clean_names() %>%
       left_join(provinces %>% select(province, province_code), by = c("geo" = "province")) %>%
       relocate(province_code, .after = geo) %>%
       arrange(month_year, geo)

### Vehicles entering Canada -----------------------------------------------------------

# Get vehicles entering Canada, province-month panel

vehicles_entering_canada_monthly <-
       vehicles_entering_canada_monthly_table %>%
       filter(geo != "Canada",
              vehicle_licence_plate == "Vehicles entering Canada",
              vehicle_type == "Vehicles") %>%
       select(month_year = ref_date,
              geo,
              scale = scalar_factor,
              vehicles = value) %>%
       clean_names() %>%
       inner_join(provinces %>% select(province, province_code), by = c("geo" = "province")) %>%
       relocate(province_code, .after = geo) %>%
       arrange(month_year, geo)

## Electric power generation -----------------------------------------------------------

### Electric power generation, by province, monthly, seasonally adjusted -----------------------------------------------------------

# Create a panel with electric power generation, province-month

electric_power_generation_prov_monthly_1 <-
       electric_power_generation_prov_monthly_table_1 %>%
       filter(geo != "Canada",
              electric_power_components == "Overall total generation") %>%
       select(month_year = ref_date,
              geo,
              scale = scalar_factor,
              electric_power_generation = value) %>%
       clean_names() %>%
       inner_join(provinces %>% select(province, province_code), by = c("geo" = "province")) %>%
       relocate(province_code, .after = geo) %>%
       arrange(month_year, geo)    

# Create a panel with electric power generation, province-month

electric_power_generation_prov_monthly_2 <-
       electric_power_generation_prov_monthly_table_2 %>%
       filter(geo != "Canada",
              class_of_electricity_producer == "Total all classes of electricity producer",
              type_of_electricity_generation == "Total all types of electricity generation") %>%
       select(month_year = ref_date,
              geo,
              scale = scalar_factor,
              electric_power_generation = value) %>%
       clean_names() %>%
       left_join(provinces %>% select(province, province_code), by = c("geo" = "province")) %>%
       relocate(province_code, .after = geo) %>%
       arrange(month_year, geo)

# Horizontally bind electric power generation tables

electric_power_generation_prov_monthly <- 
    bind_rows(electric_power_generation_prov_monthly_1, electric_power_generation_prov_monthly_2)

## Experimental economic activity -----------------------------------------------------------

## Experimental indexes of econ activity by province -----------------------------------------------------------

# Get experimental indexes of economic activity, province-month panel

experimental_econ_activity_prov_monthly <-
       experimental_econ_activity_prov_monthly_table %>%
       filter(geo != "Canada",
              activity_index == "Simple economic activity index") %>%
       select(month_year = ref_date,
              geo,
              scale = scalar_factor,
              exp_index_econ_activity = value) %>%
       clean_names() %>%
       mutate(geo = str_remove_all(geo, "Â") %>% str_trim()) %>% 
       left_join(provinces %>% select(province, province_code), by = c("geo" = "province")) %>%
       relocate(province_code, .after = geo) %>%
       arrange(month_year, geo)

## International trade -----------------------------------------------------------

### Total international merchandise trade -----------------------------------------------------------

# Get total international merchandise imports, province-month panel

international_merchandise_imports_prov_monthly <-
       international_merchandise_trade_monthly_table %>%
       filter(geo != "Canada",
              trade == "Import",
              north_american_product_classification_system_napcs == "Total of all merchandise") %>%
       select(month_year = ref_date,
              geo,
              scale = scalar_factor,
              international_merchandise_imports = value,
              countries = principal_trading_partners) %>%
       pivot_wider(names_from = countries,
                   values_from = international_merchandise_imports,
                   names_prefix = "imports_") %>%
       clean_names() %>%
       inner_join(provinces %>% select(province, province_code), by = c("geo" = "province")) %>%
       relocate(province_code, .after = geo) %>%
       arrange(month_year, geo)

# Get total international merchandise exports, province-month panel

international_merchandise_exports_prov_monthly <-
       international_merchandise_trade_monthly_table %>%
       filter(geo != "Canada",
              trade == "Domestic export",
              north_american_product_classification_system_napcs == "Total of all merchandise") %>%
       select(month_year = ref_date,
              geo,
              scale = scalar_factor,
              international_merchandise_exports = value,
              countries = principal_trading_partners) %>%
       pivot_wider(names_from = countries,
                   values_from = international_merchandise_exports,
                   names_prefix = "exports_") %>%
       clean_names() %>%
       inner_join(provinces %>% select(province, province_code), by = c("geo" = "province")) %>%
       relocate(province_code, .after = geo) %>%
       arrange(month_year, geo)

## Housing -----------------------------------------------------------

### New housing price index -----------------------------------------------------------

# Get new housing price index, province-month panel

new_housing_price_index_prov_monthly <-
       new_housing_price_index_monthly_table %>%
       filter(geo != "Canada",
              new_housing_price_indexes == "Total (house and land)") %>%
       select(month_year = ref_date, 
              geo,
              scale = scalar_factor,
              new_housing_price_index = value) %>%
       clean_names() %>%
       inner_join(provinces %>% select(province, province_code), by = c("geo" = "province")) %>%
       relocate(province_code, .after = geo) %>%
       arrange(month_year, geo)

## Building permits -----------------------------------------------------------

building_permits_prov_monthly_1 <-
       building_permits_monthly_table_1 %>%
       filter(geo != "Canada",
              type_of_dwelling == "Total dwellings",
              area == "All areas") %>%
       select(month_year = ref_date,
              geo,
              scale = scalar_factor,
              building_permits = value) %>%
       clean_names() %>%
       inner_join(provinces %>% select(province, province_code), by = c("geo" = "province")) %>%
       relocate(province_code, .after = geo) %>%
       arrange(month_year, geo)

building_permits_prov_monthly_2 <-
       building_permits_monthly_table_2 %>%
       filter(geo != "Canada",
              type_of_building == "Total residential and non-residential",
              type_of_work == "Types of work, total",
              variables == "Number of permits") %>%
       select(month_year = ref_date,
              geo,
              scale = scalar_factor,
              building_permits = value) %>%
       clean_names() %>%
       inner_join(provinces %>% select(province, province_code), by = c("geo" = "province")) %>%
       relocate(province_code, .after = geo) %>%
       arrange(month_year, geo)

building_permits_prov_monthly <-
       bind_rows(building_permits_prov_monthly_1, building_permits_prov_monthly_2)

# Data preparation for other GOC data -----------------------------------------------------------

## Insolvency statistics -----------------------------------------------------------

# Get insolvency statistics for every province and territory.

### Reading the data -----------------------------------------------------------

# This data comes as an Excel file but the format isn't friendly with database loading, so I prepare loading parameters to later read the data into R with readxl. 

first_two_cols <- c("province",
                    "type_of_insolvency")

# Create a vector of dates from january 1987 to december 2023

dates <- seq.Date(from = as.Date("1987-01-01"), to = as.Date("2023-12-01"), by = "month")

# Create a vector of the column names

column_names <- c(first_two_cols, as.character(dates))

# Do the column types (which define column classes) for the loading of the data
# All should be integers except for the first two columns

column_types <- c("text", "text", rep("numeric", length(dates)))

# Read the data into R the XL file with readxl and prepare accordingly

insolvency_monthly_data <-
    read_excel("data/goc/raw/historical_insolvency_statistics_monthly_dec_2023_0.xlsx", 
                sheet = 1,
                col_names = column_names,
                col_types = column_types,
                skip = 1) %>% 
     fill(province, .direction = "down") %>% 
     mutate(province = str_extract(province, "^[^/]+"),
            type_of_insolvency = str_extract(type_of_insolvency, "^[^/]+")) %>% 
     pivot_longer(cols = -c(province, type_of_insolvency), 
                  names_to = "month_year", 
                  values_to = "insolvencies",
                  names_transform = list(month_year = as.Date)) %>% 
     filter(province != "Canada") %>% 
     left_join(provinces %>% select(province, province_code), by = "province") %>%
     relocate(province_code, .before = province) %>%
     arrange(province, month_year, type_of_insolvency)

# Reshape to wider format, keeping only total insolvencies and bankruptcies, business insolvencies and bankruptcies. 

insolvency_prov_month <-
    insolvency_monthly_data %>% 
    filter(type_of_insolvency %in% c("Total Insolvencies", "Total Bankruptcies", "Business Bankruptcies", "Business Insolvencies")) %>% 
    pivot_wider(names_from = type_of_insolvency, 
                values_from = insolvencies) %>%
    clean_names() %>% 
    select(-province)

# Joining monthly data together -----------------------------------------------------------

# Join all monthly data together in a single dataframe

explanatory_vars_province_month_panel <-
       lfs_pop_province_monthly %>% 
       select(month_year, province_code, total_pop) %>%
       left_join(lfs_emp_province_monthly %>% select(month_year, province_code, total_emp, emp_males, emp_females), by = c("month_year", "province_code")) %>%
       left_join(lfs_full_emp_province_monthly %>% select(month_year, province_code, total_full_emp), by = c("month_year", "province_code")) %>%
       left_join(lfs_part_emp_province_monthly %>% select(month_year, province_code, total_part_emp), by = c("month_year", "province_code")) %>%
       left_join(lfs_unem_province_monthly %>% select(month_year, province_code, total_unem), by = c("month_year", "province_code")) %>%
       left_join(lfs_emp_rate_province_monthly %>% select(month_year, province_code, total_emp_rate), by = c("month_year", "province_code")) %>%
       left_join(lfs_total_wages_prov %>% select(month_year, province_code, total_wage, total_wage_males, total_wage_females), by = c("month_year", "province_code")) %>%
       left_join(lfs_average_hourly_wage %>% select(month_year, province_code, total_avg_wage, avg_wage_males, avg_wage_females), by = c("month_year", "province_code")) %>%
       left_join(lfs_median_hourly_wage %>% select(month_year, province_code, total_median_wage, median_wage_males, median_wage_females), by = c("month_year", "province_code")) %>%
       left_join(lfs_usual_total_hours_worked_prov %>% select(month_year, province_code, total_usual_hours), by = c("month_year", "province_code")) %>%
       left_join(lfs_usual_avg_hours_worked_prov %>% select(month_year, province_code, average_usual_hours, average_usual_hours_males, average_usual_hours_females), by = c("month_year", "province_code")) %>%
       left_join(lfs_actual_total_hours_worked_prov %>% select(month_year, province_code, total_actual_hours), by = c("month_year", "province_code")) %>%
       left_join(lfs_actual_avg_hours_worked_prov %>% select(month_year, province_code, average_actual_hours, average_actual_hours_males, average_actual_hours_females), by = c("month_year", "province_code")) %>%
       left_join(lfs_avg_job_tenure_prov_monthly %>% select(month_year, province_code, total_avg_tenure), by = c("month_year", "province_code")) %>% 
       left_join(ei_claims_prov_monthly %>% select(month_year, province_code, ei_claims), by = c("month_year", "province_code")) %>%
       left_join(cpi_all_items_prov_monthly %>% select(month_year, province_code, cpi), by = c("month_year", "province_code")) %>%
       left_join(retail_trade_sales_prov_monthly %>% select(month_year, province_code, retail_sales), by = c("month_year", "province_code")) %>%
       left_join(wholesale_trade_sales_prov_monthly %>% select(month_year, province_code, wholesale_sales), by = c("month_year", "province_code")) %>%
       left_join(manufacturing_sales_prov_monthly %>% select(month_year, province_code, manufacturing_sales), by = c("month_year", "province_code")) %>%
       left_join(food_services_sales_prov_monthly %>% select(month_year, province_code, food_services_receipts), by = c("month_year", "province_code")) %>%
       left_join(international_travellers_canada_monthly %>% select(month_year, province_code, travellers), by = c("month_year", "province_code")) %>%
       left_join(vehicles_entering_canada_monthly %>% select(month_year, province_code, vehicles), by = c("month_year", "province_code")) %>%
       left_join(electric_power_generation_prov_monthly %>% select(month_year, province_code, electric_power_generation), by = c("month_year", "province_code")) %>% 
       left_join(experimental_econ_activity_prov_monthly %>% select(month_year, province_code, exp_index_econ_activity), by = c("month_year", "province_code")) %>%
       left_join(international_merchandise_imports_prov_monthly %>% select(-geo, scale), by = c("month_year", "province_code")) %>%
       left_join(international_merchandise_exports_prov_monthly %>% select(-geo, scale), by = c("month_year", "province_code")) %>%
       left_join(new_housing_price_index_prov_monthly %>% select(month_year, province_code, new_housing_price_index), by = c("month_year", "province_code")) %>% 
       left_join(building_permits_prov_monthly %>% select(month_year, province_code, building_permits), by = c("month_year", "province_code")) %>%
       left_join(insolvency_prov_month, by = c("month_year", "province_code")) %>%
       arrange(month_year, province_code)

# Exporting province-month data -----------------------------------------------------------

# Export the province-month data to a csv file 

write_csv(explanatory_vars_province_month_panel, "data/explanatory_vars_province_month_panel.csv")
