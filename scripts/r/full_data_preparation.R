# R Script: Full Dataset Preparation
# ECON899 MA Paper 
# SFU Economics
# Daniel Sanchez
# Spring 2024 

# This dataset prepares the full dataset for the empirical analysis.

# Preliminaries -----------------------------------------------------------

# Load packages

library(dplyr)
library(readr)
library(forcats)
library(lubridate)
library(tidyr)

# Define the valid date for the IP Horizons data (December 2021)

valid_date <- ymd("2021-12-31")

# Define a treatment date (month-year the AITC was passed)

treatment_start_date <- ymd("2016-08-01")
treatment_group <- "AB"

# Load the data -----------------------------------------------------------

## Explained/Dependent variables -----------------------------------------------------------

# Load all the processed patents data (explained variables)

patents_main <- readRDS("data/patents/processed/patents_main.rds") # "Main" data, prepared from the main table downloaded from IP Horizons

patents_interested_parties <- readRDS("data/patents/processed/patents_interested_parties.rds") # "Interested parties" data, prepared from the interested parties tables downloaded from IP Horizons

patents_inventors <- readRDS("data/patents/processed/patents_inventors.rds") # Only inventors data, prepared from the interested parties tables downloaded from IP Horizons

patents_owners <- readRDS("data/patents/processed/patents_owners.rds") # Only owners data, prepared from the interested parties tables downloaded from IP Horizons

patents_applicants <- readRDS("data/patents/processed/patents_applicants.rds") # Only applicants data, prepared from the interested parties tables downloaded from IP Horizons

patent_province_mapping <- readRDS("data/patents/processed/patent_province_mapping.rds") # Mapping of patents to provinces based on % of interested parties

## Explanatory variables/Regressors -----------------------------------------------------------

# Load explanatory variables panel dataset

explanatory_province_month_panel_df <- read_csv("data/explanatory_vars_province_month_panel.csv", show_col_types = F)

## Other data --------------------------------------------------------------------------------------

# Load province codes and names

province_codes <- read_csv("data/other/province_codebook.csv", show_col_types = F)

# Main patent data --------------------------------------------------------------------------------------

# Get the number of patents per province and month based on my mapping of patents to provinces
# First do them based on filing date, I will later do them based on grant date

patents_per_province_month_filing <- 
       patents_main %>% 
       left_join(patent_province_mapping, by = "patent_number") %>% 
       group_by(province_code_clean, filing_month_year) %>% 
       summarise(patents_filed = n(),
                 foreign_parties = sum(foreign_patents)) %>% 
       ungroup() %>% 
       arrange(province_code_clean, desc(filing_month_year))

# Get the number of patents per province and month based on grant date

patents_per_province_month_grant <- 
       patents_main %>% 
       left_join(patent_province_mapping, by = "patent_number") %>% 
       group_by(province_code_clean, grant_month_year) %>% 
       summarise(patents_granted = n()) %>% 
       ungroup() %>% 
       arrange(province_code_clean, desc(grant_month_year))

# Create a panel of provinces and months for the main patent data with expand_grid

months <- seq.Date(from = min(patents_main$filing_month_year, na.rm = T), to = valid_date, by = "month")

provinces <- province_codes$province_code

patents_per_province <-
       expand_grid(province_code = provinces, month_year = months) %>% 
       left_join(patents_per_province_month_filing, by = c("province_code" = "province_code_clean", "month_year" = "filing_month_year")) %>% 
       left_join(patents_per_province_month_grant, by = c("province_code" = "province_code_clean", "month_year" = "grant_month_year")) %>% 
       replace_na(list(patents_filed = 0, patents_granted = 0, foreign_parties = 0)) 

# Interested parties  --------------------------------------------------------------------------------------

# Add the dates of filing date of the application to the interested parties data from the main dataset. 
# This means joining the main data with the interested parties data through the patent number without having to do strange mappings of patents to provinces. 

interested_parties_with_dates <-  
       patents_interested_parties %>%
       left_join(patents_main %>% select(patent_number, filing_month_year), 
                 by = "patent_number")

# Do the same thing for the inventors data

inventors_with_dates <-  
       patents_inventors %>%
       left_join(patents_main %>% select(patent_number, filing_month_year), 
                 by = "patent_number")

# Do the same thing for the owners data

owners_with_dates <-  
       patents_owners %>%
       left_join(patents_main %>% select(patent_number, filing_month_year), 
                 by = "patent_number")

# Do the same thing for the applicants data

applicants_with_dates <-  
       patents_applicants %>%
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

# Count the number of inventors per province and month. Filter out non-Canadian provinces.

inventors_province_month <-
       inventors_with_dates %>%
       filter(country_mapped_to_province == 'Canada') %>%
       group_by(province_code_clean, filing_month_year) %>%
       summarise(n_inventors = n()) %>%
       ungroup()  %>% 
       arrange(province_code_clean, desc(filing_month_year))

# Count the number of owners per province and month. Filter out non-Canadian provinces.

owners_province_month <-
       owners_with_dates %>%
       filter(country_mapped_to_province == 'Canada') %>%
       group_by(province_code_clean, filing_month_year) %>%
       summarise(n_owners = n()) %>%
       ungroup()  %>% 
       arrange(province_code_clean, desc(filing_month_year))

# Count the number of applicants per province and month. Filter out non-Canadian provinces.

applicants_province_month <-
       applicants_with_dates %>%
       filter(country_mapped_to_province == 'Canada') %>%
       group_by(province_code_clean, filing_month_year) %>%
       summarise(n_applicants = n()) %>%
       ungroup()  %>% 
       arrange(province_code_clean, desc(filing_month_year))

# Full dataset preparation --------------------------------------------------------------------------------------

# Finalize the dataset by merging all explained and explanatory variables
# Also, define treatment groups and periods based on the treatment date
# Further, filter the dependent whole dataset to before the valid period for IP Horizons data (before december 2021)
# The periods will be defined as a centered time variable: the number of months since the treatment date.
# t = 0 is the treatment date, t < 0 is the pre-treatment period, and t > 0 is the post-treatment period.
# Also create any transformations of variables required for the analysis
# I also redefine the reference level of the factors to be the control group and the pre-treatment period

df <-
       patents_per_province %>%
       left_join(interested_parties_province_month, by = c("province_code" = "province_code_clean", "month_year" = "filing_month_year")) %>% # All interested parties
       left_join(inventors_province_month, by = c("province_code" = "province_code_clean", "month_year" = "filing_month_year")) %>%  # Only inventors
       left_join(owners_province_month, by = c("province_code" = "province_code_clean", "month_year" = "filing_month_year")) %>%  # Only owners
       left_join(applicants_province_month, by = c("province_code" = "province_code_clean", "month_year" = "filing_month_year")) %>%  # Only applicants
       left_join(explanatory_province_month_panel_df, by = c("province_code", "month_year")) %>% # Statistics Canada data
       mutate(province_code = as_factor(province_code),
              periods = interval(treatment_start_date, month_year)/months(1),
              ln_patents_filed = log(patents_filed + 1),
              ln_patents_granted = log(patents_granted + 1),
              ln_patents_filed_1 = log(patents_filed + 1),
              ln_patents_granted_1 = log(patents_granted + 1),
              patent_parties = n_interested_parties,
              inventors = n_inventors,
              owners = n_owners,
              applicants = n_applicants,
              ln_foreign_parties = log(foreign_parties),
              ln_foreign_parties_1 = log(foreign_parties + 1),
              ln_parties = log(n_interested_parties),
              ln_parties_1 = log(n_interested_parties + 1),
              ln_inventors = log(n_inventors),
              ln_inventors_1 = log(n_inventors + 1),
              ln_owners = log(n_owners),
              ln_owners_1 = log(n_owners + 1),
              ln_applicants = log(n_applicants),
              ln_applicants_1 = log(n_applicants + 1),
              treatment = if_else(province_code == treatment_group, "Treatment", "Control") %>% as.factor() %>% relevel("Control"),
              post = if_else(month_year >= treatment_start_date , "Post", "Pre") %>% as.factor() %>% relevel("Pre"))  %>%
       arrange(province_code, month_year) 

# Export the data --------------------------------------------------------------------------------------

# Save the data to an RDS file

saveRDS(interested_parties_province_month, "data/patents/processed/interested_parties_province_month.rds")

# Save the final dataset to an RDS file

saveRDS(df, "data/full_dataset.rds")

# Save the final dataset to a CSV file, for other software

write_csv(df, "data/full_dataset.csv")