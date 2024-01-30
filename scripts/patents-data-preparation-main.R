# # ECON899 MA Paper 
# SFU Economics
# Daniel Sanchez
# Spring 2024 

# This script prepares the patents main data files for later processing.

# Preliminaries -----------------------------------------------------------

# Load packages

library(readr)
library(dplyr)
library(janitor)

# Loading the data --------------------------------------------------------

# Load the main data file, defining loading parameters for the read_csv function

# Column types (and selecting which to load, those which are not needed are "skip")

main_columns <- c('patent_number'= 'integer',
                  'filing_date' = 'D',
                  'grant_date' = 'D',
                  'application_status_code' = 'c',
                  'application_type_code' = 'c',
                  'french' = 'skip',
                  'patent_title_en' = 'c',
                  'bibliographic' = 'skip',
                  'country_publication' = 'c',
                  'document_kind' = 'c',
                  'examination' = 'skip',
                  'filing_country' = 'c',
                  'filing_language' = 'c',
                  'willing_to_sell' = 'integer',
                  'pct_application_number' = 'c',
                  'pct_publication_number' = 'c',
                  'pct_publication_date' = 'D',
                  'parent_application_number' = 'integer',
                  'pct_article' = 'skip',
                  'pct_section' = 'skip',
                  'pct_publication_country' = 'c',
                  'publication_kind' = 'c',
                  'printed_as_amended_country' = 'c')

column_names <- names(main_columns)

# Load the data 

patents_main1 <- read_delim("data/patents/raw/PT_main_1_to_2000000_2023-06-13.csv",
                            skip = 1,
                            delim = "|",
                            col_types = main_columns,
                            col_names = column_names,
                            locale = locale(encoding = "UTF-8"))

patents_main2 <- read_delim("data/patents/raw/PT_main_2000001_to_4000000_2023-06-13.csv",
                            skip = 1,
                            delim = "|",
                            col_types = main_columns,
                            col_names = column_names,
                            locale = locale(encoding = "UTF-8"))

# Combine the two data frames

patents_main <- bind_rows(patents_main1, patents_main2)

# EDA ---------------------------------------------------------------------

# Application status code

patents_main %>%
  group_by(application_status_code) %>%
  summarise(n = n()) %>%
  mutate(pct = n / sum(n)) %>%
  arrange(desc(n))

# PCT applications (international patent applications)

patents_main %>%
  group_by(application_type_code) %>%
  summarise(n = n()) %>%
  mutate(pct = n / sum(n)) %>%
  arrange(desc(n))

# Country of publication

patents_main %>%
  group_by(country_publication) %>%
  summarise(n = n()) %>%
  mutate(pct = n / sum(n)) %>%
  arrange(desc(n))

# Document kind type

patents_main %>%
  group_by(document_kind) %>%
  summarise(n = n()) %>%
  mutate(pct = n / sum(n)) %>%
  arrange(desc(n))

# Filing country

patents_main %>%
  group_by(filing_country) %>%
  summarise(n = n()) %>%
  mutate(pct = n / sum(n)) %>%
  arrange(desc(n))

# Not all are initially filed in Canada, but all are published in Canada.

# Parent application number

patents_main %>%
  mutate(has_parent = if_else(is.na(parent_application_number), 'No', 'Yes'))  %>% 
  group_by(has_parent) %>%
  summarise(n = n()) %>%
  mutate(pct = n / sum(n)) %>%
  arrange(desc(n))

# Most don't have a parent

# PCT publication country

patents_main %>%
  group_by(pct_publication_country) %>%
  summarise(n = n()) %>%
  mutate(pct = n / sum(n)) %>%
  arrange(desc(n))

# Exporting the data ------------------------------------------------------

# Export the data as an RDS file, which is faster to load than a CSV

saveRDS(patents_main, "data/patents/processed/patents_main.rds")