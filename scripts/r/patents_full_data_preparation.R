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

patents_agents <- readRDS("data/patents/processed/patents_agents.rds")

# Join patents to agents --------------------------------------------------

patents_main_and_agents <- 
       patents_main  %>% 
       select(patent_number,
              filing_date,
              grant_date)  %>% 
       left_join(patents_agents  %>% select(patent_number, party_country_code, party_province, party_province_code),
                 by = "patent_number")

# Only one agent per patent, so the mapping to province is perfectly done. 

## Patents by province, using the agent's province -------------------------

# Group patents at the province level, filtering out non-Canadian patents and patents before 

patents_by_province <- 
       patents_main_and_agents  %>% 
       group_by(party_province_code)  %>% 
       summarise(patents = n())  %>% 
       arrange(desc(patents))