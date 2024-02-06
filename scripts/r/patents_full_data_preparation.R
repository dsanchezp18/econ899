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

## Province-month panel, using the agent's province -------------------------

# Build a province-month panel, using the agent's province and filtering out non-Canadian patents

patents_province_month_agent <-
       patents_main_and_agents  %>% 
       filter(party_country_code == "CA")  %>% 
       mutate(grant_month = floor_date(grant_date, "month"))  %>% 
       group_by(party_province, grant_month)  %>% 
       summarise(patents = n())  %>% 
       ungroup()  %>% 
       arrange(party_province, desc(grant_month))