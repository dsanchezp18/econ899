# R Script: Patents Data Download
# ECON899 MA Paper 
# SFU Economics
# Daniel Sanchez
# Spring 2024 

# This script downloads the raw data files from the IP Horizons (CIPO) website.

# Preliminaries -----------------------------------------------------------

# Load packages

library(readr)
library(dplyr)

# Set a larger timeout options

options(timeout = 300)

# Downloading the data -----------------------------------------------------------

# Download the raw data files from the IP Horizons (CIPO) website

# 1. Data dictionary

url <- "https://opic-cipo.ca/cipo/client_downloads/IP_Horizon_Resources/PT_Data_Dictionary.zip"

download.file(url, destfile = "data/patents/raw/zip/PT_Data_Dictionary.zip")

# 2. Main

url <- c("https://opic-cipo.ca/cipo/client_downloads/patent_CSV_2023_06_16/PT_main_1_to_2000000_2023-06-13.zip",
        "https://opic-cipo.ca/cipo/client_downloads/patent_CSV_2023_06_16/PT_main_2000001_to_4000000_2023-06-13.zip")

mapply(download.file, url, destfile = file.path("data/patents/raw/zip", basename(url)))

# 3. Interested parties

url <- c("https://opic-cipo.ca/cipo/client_downloads/patent_CSV_2023_06_16/PT_interested_party_1_to_2000000_2023-06-13.zip",
        "https://opic-cipo.ca/cipo/client_downloads/patent_CSV_2023_06_16/PT_interested_party_2000001_to_4000000_2023-06-13.zip")

mapply(download.file, url, destfile = file.path("data/patents/raw/zip", basename(url)))

# 4. IPC classifications

url<- c("https://opic-cipo.ca/cipo/client_downloads/patent_CSV_2023_06_16/PT_IPC_classification_1_to_2000000_2023-06-13.zip",
        "https://opic-cipo.ca/cipo/client_downloads/patent_CSV_2023_06_16/PT_IPC_classification_2000001_to_4000000_2023-06-13.zip")

mapply(download.file, url, destfile = file.path("data/patents/raw/zip", basename(url)))