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

govcan_dl_resources(insolvency_monthly_package_resources, path = "data/goc/raw")