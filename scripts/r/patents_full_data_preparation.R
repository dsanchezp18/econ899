# R Script: Full Dataset Preparation
# ECON899 MA Paper 
# SFU Economics
# Daniel Sanchez
# Spring 2024 

# This dataset prepares the full dataset for the empirical analysis.

# Preliminaries -----------------------------------------------------------

# Load packages

library(dplyr)

# Load the data -----------------------------------------------------------

# Load all the processed patents data

patents_main <- readRDS("data/patents/processed/patents_main.rds")

patents_interested_parties <- readRDS("data/patents/processed/patents_interested_parties.rds")

# Interested parties and main data (dependent variable redefinition) --------------------------------------------

# Add the dates of filing and grant of application to the interested parties data from the main dataset. 
# This means joining the main data with the interested parties data through the patent number without having to do strange mappings of patents to provinces. 
# The dependent variable then becomes the number of interested parties per patent.

interested_parties_with_dates <-  
       patents_interested_parties %>%
       left_join(patents_main %>% select(patent_number, filing_month_year), 
                 by = "patent_number")

## Province-month panel data --------------------------------------------------------------------------------------

# Count the number of interested parties per province and month. Filter out non-Canadian provinces.

interested_parties_province_month <- 
       interested_parties_with_dates %>%
       filter(country_mapped_to_province == 'Canada') %>%
       group_by(province_code_clean, filing_month_year) %>%
       summarise(n_interested_parties = n()) %>%
       ungroup()  %>% 
       arrange(province_code_clean, desc(filing_month_year)) 

# Export the data --------------------------------------------------------------------------------------

# Save the data to an RDS file

saveRDS(interested_parties_province_month, "data/patents/processed/interested_parties_province_month.rds")


