# R Script: Full Dataset Preparation
# ECON899 MA Paper 
# SFU Economics
# Daniel Sanchez
# Spring 2024 

# This dataset prepares the full dataset for the empirical analysis.

# Preliminaries -----------------------------------------------------------

# Load packages

library(dplyr)
library(forcats)
library(lubridate)

# Define a treatment date (month-year the AITC was passed)

treatment_start_date <- ymd("2016-08-01")
treatment_group <- "AB"

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

# Full dataset preparation --------------------------------------------------------------------------------------

# Finalize the dataset by merging all of the work done before and defining treatment groups and periods
# The periods will be defined as a centered time variable: the number of months since the treatment date.
# t = 0 is the treatment date, t < 0 is the pre-treatment period, and t > 0 is the post-treatment period.
# Also create any transformations of variables required for the analysis
# I also redefine the reference level of the factors to be the control group and the pre-treatment period

df <-
       interested_parties_province_month  %>% 
       transmute(province = as_factor(province_code_clean),
                 filing_month_year,
                 periods = interval(treatment_start_date, filing_month_year)/months(1),
                 patent_parties = n_interested_parties,
                 ln_parties = log(n_interested_parties),
                 ln_parties_1 = log(n_interested_parties + 1),
                 treatment = if_else(province_code_clean == treatment_group, "Treatment", "Control") %>% as.factor()  %>% relevel("Control"),
                 post = if_else(filing_month_year >= treatment_start_date , "Post", "Pre") %>% as.factor() %>% relevel("Pre"))  %>% 
       arrange(province, filing_month_year)

# Export the data --------------------------------------------------------------------------------------

# Save the data to an RDS file

saveRDS(interested_parties_province_month, "data/patents/processed/interested_parties_province_month.rds")

# Save the final dataset to an RDS file

saveRDS(df, "data/full_dataset.rds")
