# # ECON899 MA Paper 
# SFU Economics
# Daniel Sanchez
# Spring 2024 

# This script prepares the patents interested parties data files for later processing.

# Preliminaries -----------------------------------------------------------

# Load packages

library(readr)
library(dplyr)
library(janitor)

# Loading the data --------------------------------------------------------

# Define load parameters for loading the interested parties data

# Define column names and types to load. Skip those that are not needed

parties_columns <- c('patent_number' = 'i',
                     'agent_type' = 'c',
                     'applicant_type' = 'c',
                     'interested_party_type_code' = 'c',
                     'interested_party_type' = 'c',
                     'owner_enable_date' = 'D',
                     'ownership_end_date' = 'D',
                     'party_name' = 'c',
                     'address_l1' = 'c',
                     'address_l2' = 'c',
                     'address_l3' = 'c',
                     'address_l4' = 'c',
                     'address_l5' = 'c',
                     'party_city' = 'c',
                     'party_province' = 'c',
                     'party_postal_code' = 'c',
                     'party_country_code' = 'c',
                     'party_country' = 'c')

parties_column_names <- names(parties_columns)

# Load the data

parties_int1 <- read_delim("data/patents/raw/PT_interested_party_1_to_2000000_2023-06-13.csv",
                            skip = 1,
                            delim = "|",
                            col_types = parties_columns,
                            col_names = parties_column_names,
                            locale = locale(encoding = "UTF-8"))

spec(parties_int1)

