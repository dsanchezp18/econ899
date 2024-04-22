# R Script: Descriptive Statistics for the quarterly sample
# ECON899 MA Paper 
# SFU Economics
# Daniel Sanchez
# Spring 2024 

# This script prepares a table with the data sources used in the paper. 

# Preliminaries -----------------------------------------------------------

# Load libraries 

library(dplyr)
library(kableExtra)
library(tinytable) # Install using remotes::install_github("vincentarelbundock/tinytable")
library(modelsummary)
library(lubridate)

# Loading data -----------------------------------------------------------

# Load full dataset

# Load full dataset

df_full <- readRDS("data/full_dataset_monthly.rds")

# Define treatment start date

treatment_start_date <- ymd("2016-04-01")

# Define valid start and end dates

start_date <- ymd("2001-01-01")

end_date <- ymd("2021-06-01")

# Define valid start and end periods

start_period <- interval(start_date, treatment_start_date)/months(1)

end_period <- interval(treatment_start_date, end_date)/months(1)

# Define df (relevant data) within the valid start period and end period already defined

df <- 
    df_full %>%
    filter(month_year %>% between(start_date, end_date))

# Select variables for the descriptive statistic tables

df_descriptive_explained <-
    df %>% 
    select(province_code,
           `Treatment` = treatment,
           `Period` = post,
           `Ln +1 Patent applications` = ln1_patents_filed,
           `Ln +1 Interested parties` = ln1_interested_parties,
           `Ln +1 Inventors` = ln1_inventors,
           `Ln +1 Applicants` = ln1_applicants,
           `Ln +1 Owners` = ln1_owners,
           `Ln +1 Total population` = ln_total_pop,
           `Ln +1 Foreign patent parties` = ln1_foreign_parties,
           `Ln +1 Section A applications` = ln1_patents_A,
           `Ln +1 Section B applications` = ln1_patents_B,
           `Ln +1 Section C applications` = ln1_patents_C,
           `Ln +1 Section D applications` = ln1_patents_D,
           `Ln +1 Section E applications` = ln1_patents_E,
           `Ln +1 Section F applications` = ln1_patents_F,
           `Ln +1 Section G applications` = ln1_patents_G,
           `Ln +1 Section H applications` = ln1_patents_H,
           `Ln +1 Multiple section applications` = ln1_patents_Multiple)

df_descriptive_all <-
    df %>% 
    select(`Ln +1 Patent applications` = ln1_patents_filed,
           `Ln Full-time employment` = ln_total_full_emp,
           `Ln Median wage` = ln_total_median_wage,
           `CPI` = cpi,
           `Ln +1 Business insolvencies` = ln1_business_insolvencies,
           `Ln Intl. exports` = ln_exports_all_countries,
           `Ln Intl. imports` = ln_imports_all_countries,
           `Ln Retail sales` = ln_retail_sales,
           `Ln Wholesale sales` = ln_wholesale_sales,
           `Ln Manufacturing sales` = ln_manufacturing_sales,
           `Ln International travellers` = ln_travellers,
           `Ln Arriving vehicles` = ln_vehicles,
           `Ln Electric power generation` = ln_electric_power_generation,
           `Ln Average actual hours` = ln_average_actual_hours,
           `New housing price index` = new_housing_price_index,
           `Ln Food services receipts` = ln_food_services_receipts,
           `Ln Average job tenure` = ln_total_avg_tenure,
    )

# Descriptive statistics for variables in the sample -----------------------

# Create a table of descriptive statistics for all variables 

sink("output/tables/descriptive_statistics_monthly.tex")

datasummary(All(df_descriptive_all) ~ (Mean + SD + Min + Median + Max),
            data = df_descriptive_all,
            fmt = "%.3f",
            booktabs = T,
            output = "latex_tabular")

sink()

# Descriptive statistics for explained variables ---------------------------

# Create a table of differences in means and medians for the explained variables

sink("output/tables/descriptive_statistics_explained_monthly.tex")

datasummary(Treatment*All(df_descriptive_explained) ~ Mean*Period,
            data = df_descriptive_explained,
            fmt = "%.3f",
            booktabs = T,
            output = "latex_tabular"
    )

sink()


