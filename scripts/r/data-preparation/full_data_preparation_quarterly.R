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

# Define a treatment date (month-year the AITC was passed)

treatment_start_date <- ymd("2016-08-01")

# Define a treatment period (Alberta)

treatment_group <- "AB"

# Load the data -----------------------------------------------------------

## Explained/Dependent variables -----------------------------------------------------------

# Load all the processed patents data (explained variables)

patents_main <- readRDS("data/patents/processed/patents_main.rds") # "Main" data, prepared from the main table downloaded from IP Horizons

patents_interested_parties <- readRDS("data/patents/processed/patents_interested_parties.rds") # "Interested parties" data, prepared from the interested parties tables downloaded from IP Horizons

patents_inventors <- readRDS("data/patents/processed/patents_inventors.rds") # Only inventors data, prepared from the interested parties tables downloaded from IP Horizons

patents_owners <- readRDS("data/patents/processed/patents_owners.rds") # Only owners data, prepared from the interested parties tables downloaded from IP Horizons

patents_applicants <- readRDS("data/patents/processed/patents_applicants.rds") # Only applicants data, prepared from the interested parties tables downloaded from IP Horizons

patent_province_mapping <- readRDS("data/patents/processed/patent_province_mapping.rds") # Mapping of patents to provinces based on % of interested parties

patents_ipc_sections <- readRDS("data/patents/processed/patent_ipc_sections.rds") # IPC sections data, prepared from the IPC classification tables downloaded from IP Horizons
