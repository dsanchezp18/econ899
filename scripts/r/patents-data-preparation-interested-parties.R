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

# Define the province codes for Canada (as per WIPO ST.3)

province_codebook <- tibble(
  province_code = c("AB", "BC", "MB", "NB", "NL", "NS", "NT", "NU", "ON", "PE", "QC", "SK", "YT"),
  province = c("Alberta", "British Columbia", "Manitoba", "New Brunswick", "Newfoundland and Labrador", "Nova Scotia", "Northwest Territories", "Nunavut", "Ontario", "Prince Edward Island", "Quebec", "Saskatchewan", "Yukon"),
  province_code_wipo = c("A1", "B1", "M1", "N3", "N1","N2","N4", "N5", "O1", "Q1", "Q2", "S1", "Y1"),
  country_mapped_to_province = c("Canada", "Canada", "Canada", "Canada", "Canada", "Canada", "Canada", "Canada", "Canada", "Canada", "Canada", "Canada", "Canada")
)

# Define US state codes (as per WIPO ST.3)

state_codebook <- tibble(
    state_code = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"),
    state = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming"),
    country_mapped_to_province= rep("United States", 50),
)

# Define a state-province codebook for all provinces

state_province_codebook <-
    state_codebook %>%
    transmute(country_mapped_to_province,
              province = state, 
              province_code = state_code,
              province_code_wipo = province_code)  %>% 
    bind_rows(province_codebook)  %>% 
    relocate(country_mapped_to_province)

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
           applicant_type = applicant_type  %>% na_if("-2"),
           party_province_code = party_province_code %>% na_if("-1"))  %>% 
    left_join(state_province_codebook  %>% select(province_code_wipo, country_mapped_to_province), by = c("party_province_code" = "province_code_wipo")) 

# EDA ---------------------------------------------------------------------

# Look at patents by applicant type 

parties_by_applicant_type <-
    parties_int %>%
    group_by(applicant_type) %>%
    summarise(n = n()) %>%
    mutate(prop = n / sum(n))

# Most interested parties are not applicants (makes sense)

# Look at patents by interested party code

parties_by_party_code <-
    parties_int %>%
    group_by(interested_party_type_code) %>%
    summarise(n = n()) %>%
    mutate(prop = n / sum(n)) %>%
    arrange(desc(prop))

# Most are inventors, then the patent owners. 

# Countries of each interested party

parties_by_country <-
  parties_int %>%
  group_by(party_country) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n)) %>%
  arrange(desc(prop))

# Most interested parties are from the US, followed by Canada (without NAs)

# Look at provinces of each interested party

parties_by_province <-
  parties_int %>%
  group_by(party_province) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n)) %>%
  arrange(desc(prop))

# Only about 7% belong to a province in Canada. The rest are either unknown or belong to the US. Let's see this in more detail using the province code below. 

# Look at province codes of each interested party

parties_by_country_mapped_to_province <-
    parties_int %>%
    group_by(country_mapped_to_province) %>%
    summarise(n = n()) %>%
    mutate(prop = n / sum(n)) 

# 7% of parties are Canadian, 10.1 are from the US. The rest are from elsewhere. Actually unknown are just 25%. 

