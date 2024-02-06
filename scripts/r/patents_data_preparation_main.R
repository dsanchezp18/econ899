# ECON899 MA Paper 
# SFU Economics
# Daniel Sanchez
# Spring 2024 

# This script prepares the patents main data files for later processing.

# Preliminaries -----------------------------------------------------------

# Load packages

library(readr)
library(dplyr)
library(janitor)
library(lubridate)
library(ggplot2)
library(scales)

# Define the maximum date to take into account for the patents

max_date <- ymd("2021-12-31") # As defined by the "official" documentation, though it seems they do report some data for 2022

# Loading the data --------------------------------------------------------

# Load the main data file, defining loading parameters for the read_csv function

# Determining column types and selecting which to load, those which are not needed are defined as "skip"

main_columns <- c('patent_number'= 'i',
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

main_column_names <- names(main_columns)

# Load the data 

patents_main1 <- read_delim("data/patents/raw/PT_main_1_to_2000000_2023-06-13.csv",
                            skip = 1,
                            delim = "|",
                            col_types = main_columns,
                            col_names = main_column_names,
                            locale = locale(encoding = "UTF-8"))

patents_main2 <- read_delim("data/patents/raw/PT_main_2000001_to_4000000_2023-06-13.csv",
                            skip = 1,
                            delim = "|",
                            col_types = main_columns,
                            col_names = main_column_names,
                            locale = locale(encoding = "UTF-8"))

# Combine the two data frames

patents_main_raw <- 
  bind_rows(patents_main1, patents_main2)

# Clean the data ------------------------------------------------------------

# Clean the data 

patents_main <- 
  patents_main_raw  %>% 
  mutate(filing_month_year = floor_date(filing_date, "month")) %>% 
  arrange(desc(filing_month_year))

# EDA ---------------------------------------------------------------------

# Some exploratory data analysis to understand the main data file

# Application status code

patents_main %>%
  group_by(application_status_code) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n)) %>%
  arrange(desc(n))

# prop applications (international patent applications)

patents_main %>%
  group_by(application_type_code) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n)) %>%
  arrange(desc(n))

# Country of publication

patents_main %>%
  group_by(country_publication) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n)) %>%
  arrange(desc(n))

# Document kind type

patents_main %>%
  group_by(document_kind) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n)) %>%
  arrange(desc(n))

# Filing country

patents_main %>%
  group_by(filing_country) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n)) %>%
  arrange(desc(n))

# Not all are initially filed in Canada, but all are published in Canada.

# Parent application number

patents_main %>%
  mutate(has_parent = if_else(is.na(parent_application_number), 'No', 'Yes'))  %>% 
  group_by(has_parent) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n)) %>%
  arrange(desc(n))

# Most don't have a parent

# prop publication country 

patents_main %>%
  group_by(pct_publication_country) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n)) %>%
  arrange(desc(n))

# Chart the distribution of the number of patents by month-year of filing date

patents_filed_per_month <-
  patents_main %>%
  filter(filing_month_year <= max_date)  %>% 
  group_by(filing_month_year) %>%
  summarise(n = n())  %>% 
  ungroup()  %>%
  arrange(desc(filing_month_year))

patents_filed_per_month_fig <-
  patents_filed_per_month %>% 
  ggplot(aes(x = filing_month_year, y = n)) +
  geom_line() +
  labs(title = "Number of patents filed in Canada by filing date",
       subtitle = "Grouped at the monthly level",
       x = "Filing date period",
       y = "Number of patents filed",
       caption = "Note: Data obtained from Innovation, Science and Economic Development Canada (ISED).") +
  scale_x_date(date_breaks = "20 years",
               date_labels = "%Y") +
  scale_y_continuous(labels = comma, 
                     limits = c(0, 5000)) +
  theme_minimal() +
  theme(text = element_text(size = 10, family = 'serif'),
        axis.text.x = element_text(angle = 90, hjust = 1),
        axis.line.x = element_line(colour = "black"),
        plot.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1, linetype = "solid"),
        plot.caption = element_text(hjust = 0),
        panel.grid.major = element_line(linetype = "dashed"),
        panel.grid.minor = element_line(linetype = "dashed"))

patents_filed_per_month_fig

ggsave(filename = "figures/patents_filed_per_month_fig.png", 
       plot = patents_filed_per_month_fig,
       width = 17, 
       height = 10, 
       units = "cm",
       dpi = 800)

# Exporting the data ------------------------------------------------------

# Export the data as an RDS file, which is faster to load than a CSV

saveRDS(patents_main, "data/patents/processed/patents_main.rds")