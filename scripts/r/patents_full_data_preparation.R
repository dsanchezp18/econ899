# ECON899 MA Paper 
# SFU Economics
# Daniel Sanchez
# Spring 2024 

# This script prepares the full patents 

# Preliminaries -----------------------------------------------------------

# Load packages

library(dplyr)

# Load the data -----------------------------------------------------------

# Load all the processed patents data

patents_main <- readRDS("data/patents/processed/patents_main.rds")

patents_interested_parties <- readRDS("data/patents/processed/patents_interested_parties.rds")

# Join the interested parties with the main patents data ------------------

# Join by patent number and by selecting only main patents

main_patents_and_applicants <-
    patents_main  %>% 
    select(patent_number, 
           filing_date, 
           grant_date, 
           patent_title = patent_title_en, 
           filing_country)  %>%
    left_join(patents_interested_parties %>% filter(party_type == 'APPL')  %>% select(patent_number, party_name, party_country_code, party_province),
              by = "patent_number")

