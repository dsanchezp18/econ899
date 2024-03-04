# R Script: Extract and Prepare Other Data from the Government of Canada (GOC)
# ECON899 MA Paper 
# SFU Economics
# Daniel Sanchez
# Spring 2024 

# This dataset extracts and prepares other data from the Government of Canada (GOC) for the empirical analysis.

# Preliminaries -----------------------------------------------------------

# Load packages

library(dplyr)
library(lubridate)
library(readxl)
library(openxlsx)
library(readr)
library(tidyr)
library(stringr)
library(janitor)
library(rgovcan) # Needs to be installed with remotes::install_github("vlucet/rgovcan")

# Load province codebook

provinces <- read_csv("data/other/province_codebook.csv", show_col_types = F)

# Access the data through rgovcan -----------------------------------------------------------

# Look for the dataset using the package functions

rgovcan_search_insolvency <- govcan_search(keywords = c("Historic Insolvency Statistics", "Monthly", "1987"), records = 20)

# Get the CKAN package which we found during the search

ckan_package_insolvency_monthly <-rgovcan_search_insolvency[1]

# Get the id of the package

insolvency_monthly_package_id <- ckan_package_insolvency_monthly[[1]]$id

# Get the record of the package using the id

insolvency_monthly_package_record <- govcan_get_record(record_id = insolvency_monthly_package_id)

# Get the resources of the package

insolvency_monthly_package_resources <- govcan_get_resources(insolvency_monthly_package_record)

# Download the resources

govcan_dl_resources(insolvency_monthly_package_resources, path = "data/goc")

# Read the data ----------------------------------------------------------------

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
    read_excel("data/goc/historical_insolvency_statistics_monthly_dec_2023_0.xlsx", 
                sheet = 1,
                col_names = column_names,
                col_types = column_types,
                skip = 1) %>% 
     fill(province, .direction = "down") %>% 
     mutate(province = str_extract(province, "^[^/]+"),
            type_of_insolvency = str_extract(type_of_insolvency, "^[^/]+")) %>% 
     pivot_longer(cols = -c(province, type_of_insolvency), 
                  names_to = "month_year", 
                  values_to = "insolvencies") %>% 
     filter(province != "Canada") %>% 
     left_join(provinces %>% select(province, province_code), by = "province") %>%
     relocate(province_code, .before = province) %>%
     arrange(province, month_year, type_of_insolvency)

# Preparing variables for the empirical analysis --------------------------------

# Get insolvency statistics for every province and territory.
# Reshape to wider format, keeping only total insolvencies and bankruptcies, business insolvencies and bankruptcies. 

insolvency_prov_month <- 
    insolvency_monthly_data %>% 
    filter(type_of_insolvency %in% c("Total Insolvencies", "Total Bankruptcies", "Business Bankruptcies", "Business Insolvencies")) %>% 
    pivot_wider(names_from = type_of_insolvency, 
                values_from = insolvencies) %>%
    clean_names() %>% 
    select(-province)

# Export the data ----------------------------------------------------------------

write_csv(insolvency_prov_month, "data/goc/processed_insolvency_prov_month.csv")