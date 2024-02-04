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
library(stringr)

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
                     'party_province_code' = 'c',
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

parties_int2 <- read_delim("data/patents/raw/PT_interested_party_2000001_to_4000000_2023-06-13.csv",
                            skip = 1,
                            delim = "|",
                            col_types = parties_columns,
                            col_names = parties_column_names,
                            locale = locale(encoding = "UTF-8"))

# Combine 

parties_int_raw <- bind_rows(parties_int1, parties_int2)

# Clean the data ------------------------------------------------------------

parties_int <- 
    parties_int_raw %>%
    mutate(party_country = party_country %>% recode("Unknown" = NA_character_, "Country Unknown" = NA_character_),
           applicant_type = applicant_type  %>% na_if("-2"))

# EDA ---------------------------------------------------------------------

# Look at patents by applicant type 

parties_by_applicant_type <-
    parties_int %>%
    group_by(applicant_type) %>%
    summarise(n = n()) %>%
    mutate(prop = n / sum(n))

# Most interested parties are not applicants (makes sense)

# Look at patents by interested party type

# Countries of each interested party

parties_by_country <-
  parties_int %>%
  group_by(party_country) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n)) %>%
  arrange(desc(prop))

# Most interested parties are from the US, followed by Canada (without NAs)