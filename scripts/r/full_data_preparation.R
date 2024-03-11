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

# Define the valid date for the IP Horizons data (December 2021)

valid_date <- ymd("2021-12-31")

# Define a treatment date (month-year the AITC was passed)

treatment_start_date <- ymd("2016-08-01")
treatment_group <- "AB"

# Load the data -----------------------------------------------------------

# Explained/Dependent variables -----------------------------------------------------------

# Load all the processed patents data (explained variables)

patents_main <- readRDS("data/patents/processed/patents_main.rds") # "Main" data, prepared from the main table downloaded from IP Horizons

patents_interested_parties <- readRDS("data/patents/processed/patents_interested_parties.rds") # "Interested parties" data, prepared from the interested parties tables downloaded from IP Horizons

patents_inventors <- readRDS("data/patents/processed/patents_inventors.rds") # Only inventors data, prepared from the interested parties tables downloaded from IP Horizons

patents_owners <- readRDS("data/patents/processed/patents_owners.rds") # Only owners data, prepared from the interested parties tables downloaded from IP Horizons

patents_applicants <- readRDS("data/patents/processed/patents_applicants.rds") # Only applicants data, prepared from the interested parties tables downloaded from IP Horizons

patent_province_mapping <- readRDS("data/patents/processed/patent_province_mapping.rds") # Mapping of patents to provinces based on % of interested parties

# Explanatory variables/Regressors -----------------------------------------------------------

# Load Statistics Canada prepared datasets

statcan_province_month_panel_df <- read_csv("data/statcan/statcan_data_province_month_panel.csv", show_col_types = F)

# Load other data from the Government of Canada (GOC)

insolvency_province_month <- read_csv("data/goc/processed_insolvency_prov_month.csv", show_col_types = F)

# Main patent data --------------------------------------------------------------------------------------

# Get the number of patents per province and month based on my mapping of patents to provinces
# First do them based on filing date, I will later do them based on grant date

patents_per_province_month_filing <- 
       patents_main %>% 
       left_join(patent_province_mapping, by = "patent_number") %>% 
       group_by(province_code_clean, filing_month_year) %>% 
       summarise(n_patents = n()) %>% 
       ungroup() %>% 
       arrange(province_code_clean, desc(filing_month_year))

# Get the number of patents per province and month based on grant date

patents_per_province_month_grant <- 
       patents_main %>% 
       left_join(patent_province_mapping, by = "patent_number") %>% 
       group_by(province_code_clean, grant_month_year) %>% 
       summarise(n_patents = n()) %>% 
       ungroup() %>% 
       arrange(province_code_clean, desc(grant_month_year))

# Interested parties and main data (dependent variable redefinition) --------------------------------------------

# Add the dates of filing and grant of application to the interested parties data from the main dataset. 
# This means joining the main data with the interested parties data through the patent number without having to do strange mappings of patents to provinces. 
# The dependent variable then becomes the number of interested parties per patent.

interested_parties_with_dates <-  
       patents_interested_parties %>%
       left_join(patents_main %>% select(patent_number, filing_month_year), 
                 by = "patent_number")

# Do the same thing for the inventors data

inventors_with_dates <-  
       patents_inventors %>%
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

# Full dataset preparation --------------------------------------------------------------------------------------

# Finalize the dataset by merging all explained and explanatory variables
# Also, define treatment groups and periods based on the treatment date
# Further, filter the dependent whole dataset to before the valid period for IP Horizons data (before december 2021)
# The periods will be defined as a centered time variable: the number of months since the treatment date.
# t = 0 is the treatment date, t < 0 is the pre-treatment period, and t > 0 is the post-treatment period.
# Also create any transformations of variables required for the analysis
# I also redefine the reference level of the factors to be the control group and the pre-treatment period

df <-
       interested_parties_province_month  %>% # All interested parties
       left_join(inventors_province_month, by = c("province_code_clean", "filing_month_year")) %>%  # Only inventors
       transmute(province_code = as_factor(province_code_clean),
                 filing_month_year,
                 periods = interval(treatment_start_date, filing_month_year)/months(1),
                 patent_parties = n_interested_parties,
                 inventors = n_inventors,
                 other_parties = n_interested_parties - n_inventors,
                 ln_parties = log(n_interested_parties),
                 ln_parties_1 = log(n_interested_parties + 1),
                 ln_inventors = log(n_inventors),
                 ln_inventors_1 = log(n_inventors + 1),
                 ln_other_parties = log(other_parties),
                 ln_other_parties_1 = log(other_parties + 1),
                 treatment = if_else(province_code_clean == treatment_group, "Treatment", "Control") %>% as.factor() %>% relevel("Control"),
                 post = if_else(filing_month_year >= treatment_start_date , "Post", "Pre") %>% as.factor() %>% relevel("Pre"))  %>% 
       arrange(province_code, filing_month_year) %>% 
       left_join(statcan_province_month_panel_df, by = c("province_code", "filing_month_year" = "month_year")) %>% # Statistics Canada data
       left_join(insolvency_province_month, by = c("province_code", "filing_month_year" = "month_year")) %>% # Insolvency data 
       filter(filing_month_year <= valid_date)

# Export the data --------------------------------------------------------------------------------------

# Save the data to an RDS file

saveRDS(interested_parties_province_month, "data/patents/processed/interested_parties_province_month.rds")

# Save the final dataset to an RDS file

saveRDS(df, "data/full_dataset.rds")

# Save the final dataset to a CSV file, for other software

write_csv(df, "data/full_dataset.csv")