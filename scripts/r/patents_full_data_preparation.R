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

# Prepare the data ---------------------------------------------------------

# Do the joining for one patent

example_main_patent_and_parties <-
    patents_main  %>% 
    slice(1)  %>% 
    select(patent_number, 
           filing_date, 
           grant_date, 
           patent_title = patent_title_en, 
           filing_country, )  %>%
    left_join(patents_interested_parties, by = "patent_number") 