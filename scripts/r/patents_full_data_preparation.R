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

# Interested parties and main data (dependent variable redefinition) --------------------------------------------

# Add the dates of filing and grant of application to the interested parties data from the main dataset.

interested_parties_with_dates <-  
       patents_interested_parties %>%
       left_join(patents_main %>% select(patent_number, filing_month_year), by = "patent_number")

## Province-month panel data --------------------------------------------------------------------------------------

# Count the number of interested parties per province and month. Filter out non-Canadian provinces.

interested_parties_province_month <- 
       interested_parties_with_dates %>%
       filter(country_mapped_to_province == 'Canada') %>%
       group_by(province_code_clean, filing_month_year) %>%
       summarise(n_interested_parties = n()) %>%
       ungroup()  %>% 
       arrange(province_code_clean, desc(filing_month_year))

# filter between 1998 and 2021

interested_parties_province_month <- 
       interested_parties_province_month %>% 
       filter(filing_month_year > as.Date("1998-01-01"),
              filing_month_year < as.Date("2021-01-01"))

# Joining the interested parties data with the main data through applicants --------------------------------------------------------

# Join the interested parties data with the main data through the applicants. Drop patents which have more than one applicant.

# Data visualization -------------------------------------------------------------------------------------------

## Interested parties per province and month ---------------------------------------------------------------------

# Plot the number of interested parties per province and month. Consider only patents after 1950 and largest Canadian provinces

interested_parties_province_month_fig <-
       interested_parties_province_month %>% 
       filter(filing_month_year > as.Date("1950-01-01"),
              province_code_clean %in% c("AB", "QC", "ON", "BC")) %>% 
       ggplot(aes(x = filing_month_year, y = n_interested_parties, group = province_code_clean, color = province_code_clean)) +
       geom_line() +
       labs(title = "Number of interested parties per province and month",
            subtitle = "Largest Canadian provinces",
            x = "Patent filing date",
            y = "Number of interested parties in the patent",
            colour = "Province",
            caption = "Note: Data obtained from Innovation, Science and Economic Development Canada.") +
       scale_x_date(date_breaks = "20 year", 
                    date_labels = "%Y") +
       scale_y_continuous(labels = comma) +
       theme_minimal() +
       theme(text = element_text(size = 10, family = 'serif'),
              axis.text.x = element_text(angle = 90, hjust = 1),
              axis.line.x = element_line(colour = "black"),
              plot.background = element_rect(fill = "white"),
              panel.border = element_rect(colour = "black", fill = NA, linewidth = 1, linetype = "solid"),
              plot.caption = element_text(hjust = 0),
              panel.grid.major = element_line(linetype = "dashed"),
              panel.grid.minor = element_line(linetype = "dashed"),
              legend.pos = "bottom")

interested_parties_province_month_fig

ggsave("figures/interested_parties_province_month_fig.png", 
       plot = interested_parties_province_month_fig, 
       width = 17, 
       height = 10, 
       units = "cm",
       dpi = 800)

## Interested parties AB vs average non-AB ------------------------------------------------------------------------

# Plot the number of interested parties of Alberta vs non-Alberta patents over time. Consider only patents after 1950

interested_parties_province_month_ab_fig <-
       interested_parties_province_month %>%
       filter(filing_month_year > as.Date("1950-01-01")) %>%
       mutate(provinces_chart = ifelse(province_code_clean == "AB", "Alberta", "Non-Alberta")) %>%
       group_by(provinces_chart, filing_month_year) %>%
       summarise(n_interested_parties = mean(n_interested_parties, na.rm = T)) %>%
       ggplot(aes(x = filing_month_year, y = n_interested_parties, group = provinces_chart, color = provinces_chart)) +
       geom_line() +
       labs(title = "Number of interested parties per province and month",
            x = "Patent filing date",
            y = "Number of interested parties in the patent") +
       scale_x_date(date_breaks = "20 year", 
                    date_labels = "%Y") +
       scale_y_continuous(labels = comma) +
       theme_minimal() +
       theme(text = element_text(size = 10, family = 'serif'),
              axis.text.x = element_text(angle = 90, hjust = 1),
              axis.line.x = element_line(colour = "black"),
              plot.background = element_rect(fill = "white"),
              panel.border = element_rect(colour = "black", fill = NA, linewidth = 1, linetype = "solid"),
              plot.caption = element_text(hjust = 0),
              panel.grid.major = element_line(linetype = "dashed"),
              panel.grid.minor = element_line(linetype = "dashed"))

interested_parties_province_month_ab_fig

ggsave("figures/interested_parties_province_month_fig.png", 
       plot = interested_parties_province_month_ab_fig, 
       width = 17, 
       height = 10, 
       units = "cm",
       dpi = 800)


