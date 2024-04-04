# R Script: Statistics Canada Data Extraction - Quarterly
# ECON899 MA Paper 
# SFU Economics
# Daniel Sanchez
# Spring 2024 

# This script downloads quarterly data from the Statistics Canada API for later processing 

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

## Quarterly Demographic Estimates -----------------------------------------------------------

quarterly_demographic_estimates <- 
    statcan_download_data("17-10-0009-01", "eng") %>% 
    clean_names()

## Labour Force Survey (LFS) -----------------------------------------------------------
