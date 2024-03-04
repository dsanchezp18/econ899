# R Script: Data Visualization for Patents Data
# ECON899 MA Paper 
# SFU Economics
# Daniel Sanchez
# Spring 2024 

# This script is used to prepare data visualizations of the patents data.

# Preliminaries -----------------------------------------------------------

# Load libraries

library(dplyr)
library(ggplot2)
library(scales)

# Define relevant dates

start_date <- as.Date("1980-01-01")

end_date <- as.Date("2021-12-31")

treatment_starts <- as.Date("2016-08-01")

# Load the data -----------------------------------------------------------

# Load the interested parties province-month panel data

interested_parties_province_month <- readRDS("data/patents/processed/interested_parties_province_month.rds")

patents_main <- readRDS("data/patents/processed/patents_main.rds")

# Load the final dataset

df <- readRDS("data/full_dataset.rds")

# Interested parties -----------------------------------------------------------

## Interested parties per province and month ---------------------------------------------------------------------

# Plot the number of interested parties per province and month. Consider only largest Canadian provinces and all available periods

interested_parties_province_month_fig_all_dates <-
       interested_parties_province_month %>% 
       filter(province_code_clean %in% c("AB", "QC", "ON", "BC")) %>% 
       ggplot(aes(x = filing_month_year, y = n_interested_parties, group = province_code_clean, color = province_code_clean)) +
       geom_line() +
       labs(title = "Number of interested parties per province and month",
            subtitle = "Largest Canadian provinces",
            x = "Patent filing date",
            y = "Number of interested parties in the patent",
            colour = "Province",
            caption = "Note: Data obtained from Innovation, Science and Economic Development Canada.") +
       scale_x_date(date_breaks = "2 year", 
                    date_labels = "%Y") +
       scale_y_continuous(labels = comma,
                          limits = c(0, 2500)) +
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

interested_parties_province_month_fig_all_dates

ggsave("figures/interested_parties_province_month_largest_provinces_all_dates.png", 
       plot = interested_parties_province_month_fig_all_dates, 
       width = 17, 
       height = 10, 
       units = "cm",
       dpi = 800)

# Plot the number of interested parties per province and month. Consider only patents between 1980 and 2021 and largest Canadian provinces

interested_parties_province_month_fig <-
       interested_parties_province_month %>% 
       filter(filing_month_year  %>% between(start_date, end_date),
              province_code_clean %in% c("AB", "QC", "ON", "BC")) %>% 
       ggplot(aes(x = filing_month_year, y = n_interested_parties, group = province_code_clean, color = province_code_clean)) +
       geom_line() +
       labs(title = "Number of interested parties per province and month",
            subtitle = "Largest Canadian provinces",
            x = "Patent filing date",
            y = "Number of interested parties in the patent",
            colour = "Province",
            caption = "Note: Data obtained from Innovation, Science and Economic Development Canada.") +
       scale_x_date(date_breaks = "2 year", 
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

ggsave("figures/interested_parties_province_month_largest_provinces.png", 
       plot = interested_parties_province_month_fig, 
       width = 17, 
       height = 10, 
       units = "cm",
       dpi = 800)

# The same, but with natural log of the number of interested parties

interested_parties_province_month_log_fig <-
       interested_parties_province_month %>% 
       filter(filing_month_year  %>% between(start_date, end_date),
              province_code_clean %in% c("AB", "QC", "ON", "BC")) %>%
       mutate(ln_interested_parties = log(n_interested_parties)) %>%
       ggplot(aes(x = filing_month_year, y = ln_interested_parties, group = province_code_clean, color = province_code_clean)) +
       geom_line() +
       labs(title = "Log of the number of interested parties per province and month",
            subtitle = "Largest Canadian provinces",
            x = "Patent filing date",
            y = "Log of the number of interested parties in the patent",
            colour = "Province",
            caption = "Note: Data obtained from Innovation, Science and Economic Development Canada.") +
       scale_x_date(date_breaks = "2 year", 
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

interested_parties_province_month_log_fig

ggsave("figures/interested_parties_province_month_largest_provinces_log.png", 
       plot = interested_parties_province_month_log_fig, 
       width = 17, 
       height = 10, 
       units = "cm",
       dpi = 800)

## Interested parties AB vs average non-AB ------------------------------------------------------------------------

# Plot the number of interested parties of Alberta vs the mean of non-Alberta patents over time. Consider only patents after 1950

interested_parties_province_month_ab_fig <-
       interested_parties_province_month %>%
       filter(filing_month_year %>% between(start_date, end_date)) %>%
       mutate(provinces_chart = ifelse(province_code_clean == "AB", "Alberta", "Non-Alberta")) %>%
       group_by(provinces_chart, filing_month_year) %>%
       summarise(n_interested_parties = mean(n_interested_parties, na.rm = T)) %>%
       ggplot(aes(x = filing_month_year, y = n_interested_parties, group = provinces_chart, color = provinces_chart)) +
       geom_line() +
       labs(title = "Number of interested parties per province and month",
            subtitle = "Alberta vs average non-Alberta interested parties in patents filed",
            x = "Patent filing period",
            y = "Number of interested parties in filed patents",
            caption = "Note: Data obtained from Innovation, Science and Economic Development Canada.") +
       scale_x_date(date_breaks = "2 year", 
                    date_labels = "%Y") +
       scale_y_continuous(labels = comma) +
       geom_vline(xintercept = treatment_starts, linetype = "dashed", color = "#56589e") +
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

ggsave("figures/interested_parties_province_month_ab_non_ab.png", 
       plot = interested_parties_province_month_ab_fig, 
       width = 17, 
       height = 10, 
       units = "cm",
       dpi = 800)

# The same, but with natural log of the number of interested parties

interested_parties_province_month_ab_log_fig <-
       interested_parties_province_month %>%
       filter(filing_month_year %>% between(start_date, end_date)) %>%
       mutate(provinces_chart = ifelse(province_code_clean == "AB", "Alberta", "Non-Alberta")) %>%
       group_by(provinces_chart, filing_month_year) %>%
       summarise(n_interested_parties = mean(n_interested_parties, na.rm = T)) %>%
       mutate(ln_interested_parties = log(n_interested_parties)) %>%
       ggplot(aes(x = filing_month_year, y = ln_interested_parties, group = provinces_chart, color = provinces_chart)) +
       geom_line() +
       labs(title = "Log of the number of interested parties per province and month",
            subtitle = "Alberta vs average non-Alberta interested parties in patents filed",
            x = "Patent filing period",
            y = "Log of the number of interested parties in filed patents",
            caption = "Note: Data obtained from Innovation, Science and Economic Development Canada.") +
       scale_x_date(date_breaks = "2 year", 
                    date_labels = "%Y") +
       scale_y_continuous(labels = comma) +
       geom_vline(xintercept = treatment_starts, linetype = "dashed", color = "#56589e") +
       theme_minimal() +
       theme(text = element_text(size = 10, family = 'serif'),
              axis.text.x = element_text(angle = 90, hjust = 1),
              axis.line.x = element_line(colour = "black"),
              plot.background = element_rect(fill = "white"),
              panel.border = element_rect(colour = "black", fill = NA, linewidth = 1, linetype = "solid"),
              plot.caption = element_text(hjust = 0),
              panel.grid.major = element_line(linetype = "dashed"),
              panel.grid.minor = element_line(linetype = "dashed"))

interested_parties_province_month_ab_log_fig

ggsave("figures/interested_parties_province_month_ab_non_ab_log.png", 
       plot = interested_parties_province_month_ab_log_fig, 
       width = 17, 
       height = 10, 
       units = "cm",
       dpi = 800)

# Main data (patents at the national level) -----------------------------------------------------------

# Chart the distribution of the number of patents by month-year of filing date

patents_filed_per_month <-
  patents_main %>%
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

# Final dataset (df) data visualization -----------------------------------------------------------

## Interested parties, total treatment and control ----------------------------------------------------------------

# Plot the total number of interested parties in filed patents by treatment and control groups

interested_parties_total_trends_fig <-
       df %>% 
       filter(periods %>% between(start_date, end_date)) %>%
       group_by(filing_month_year, periods, treatment) %>%
       summarise(patent_parties = sum(patent_parties, na.rm = T)) %>%
       ungroup() %>% 
       ggplot(aes(x = periods, y = log(patent_parties), group = treatment, color = treatment)) +
       geom_vline(xintercept = 0, linetype = "dashed", color = "#56589e") +
       geom_line() +
       scale_y_continuous(labels = comma) +
       scale_x_continuous(breaks = seq(min(df$periods), max(df$periods), by = 50)) +
       scale_color_manual(values = c("Treatment" = "#0D3692", "Control" = "#E60F2D")) +
       labs(title = "Time series of the number of interested parties in filed patents",
            subtitle = "Natural log of the number of interested parties in filed patents",
            color = "Group",
            x = "Periods before AITC was passed",
            y = "Natural log of the number of interested parties in filed patents",
            caption = "Note: Data obtained from Innovation, Science and Economic Development Canada (ISED).") +
       theme_minimal() +
       theme(text = element_text(size = 10, family = 'serif'),
             axis.text.x = element_text(angle = 90, hjust = 1),
             axis.line.x = element_line(colour = "black"),
             plot.background = element_rect(fill = "white"),
             panel.border = element_rect(colour = "black", fill = NA, linewidth = 1, linetype = "solid"),
             plot.caption = element_text(hjust = 0),
             panel.grid.major = element_line(linetype = "dashed"),
             panel.grid.minor = element_line(linetype = "dashed"),
             legend.position = c(0.92, 0.15))

ggsave("figures/interested_parties_total_trends_fig.png", 
       plot = interested_parties_total_trends_fig, 
       width = 17, 
       height = 10, 
       units = "cm",
       dpi = 800)

# The same but with the number of inventors

inventors_total_trends_fig <-
       df %>% 
       filter(filing_month_year %>% between(start_date, end_date)) %>%
       group_by(filing_month_year, periods, treatment) %>%
       summarise(inventors = sum(inventors, na.rm = T)) %>%
       ungroup() %>% 
       ggplot(aes(x = periods, y = log(inventors), group = treatment, color = treatment)) +
       geom_vline(xintercept = 0, linetype = "dashed", color = "#56589e") +
       geom_line() +
       scale_y_continuous(labels = comma) +
       scale_x_continuous(breaks = seq(min(df$periods), max(df$periods), by = 50)) +
       scale_color_manual(values = c("Treatment" = "#0D3692", "Control" = "#E60F2D")) +
       labs(title = "Time series of the number of inventors in filed patents",
            subtitle = "Natural log of the number of inventors in filed patents",
            color = "Group",
            x = "Periods before AITC was passed",
            y = "Natural log of the number of inventors in filed patents",
            caption = "Note: Data obtained from Innovation, Science and Economic Development Canada (ISED).") +
       theme_minimal() +
       theme(text = element_text(size = 10, family = 'serif'),
             axis.text.x = element_text(angle = 90, hjust = 1),
             axis.line.x = element_line(colour = "black"),
             plot.background = element_rect(fill = "white"),
             panel.border = element_rect(colour = "black", fill = NA, linewidth = 1, linetype = "solid"),
             plot.caption = element_text(hjust = 0),
             panel.grid.major = element_line(linetype = "dashed"),
             panel.grid.minor = element_line(linetype = "dashed"),
             legend.position = c(0.92, 0.15))


ggsave("figures/inventors_total_trends_fig.png", 
       plot = inventors_total_trends_fig, 
       width = 17, 
       height = 10, 
       units = "cm",
       dpi = 800)

  df %>% 
       group_by(filing_month_year, periods, treatment) %>%
       summarise(patent_parties = sum(patent_parties, na.rm = T)) %>%
       ungroup() %>% 
       ggplot(aes(x = periods, y = log(patent_parties), group = treatment, color = treatment)) +
       geom_line() +
       scale_y_continuous(labels = comma) +
       scale_x_continuous(breaks = seq(min(df$periods), max(df$periods), by = 25)) +
       scale_color_manual(values = c("Treatment" = "#0D3692", "Control" = "#E60F2D")) +
       labs(title = "Time series of the number of interested parties in filed patents",
            subtitle = "Natural log of the number of interested parties in filed patents",
            color = "Group",
            x = "Periods before AITC was passed",
            y = "Natural log of the number of interested parties in filed patents",
            caption = "Note: Data obtained from Innovation, Science and Economic Development Canada (ISED).") +
       theme_minimal() +
       theme(text = element_text(size = 10, family = 'serif'),
             axis.text.x = element_text(angle = 90, hjust = 1),
             axis.line.x = element_line(colour = "black"),
             plot.background = element_rect(fill = "white"),
             panel.border = element_rect(colour = "black", fill = NA, linewidth = 1, linetype = "solid"),
             plot.caption = element_text(hjust = 0),
             panel.grid.major = element_line(linetype = "dashed"),
             panel.grid.minor = element_line(linetype = "dashed"),
             legend.position = c(0.92, 0.15))