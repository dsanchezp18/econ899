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
library(patchwork)
library(lubridate, warn.conflicts = FALSE)

# Define valid start and end dates

start_date <- ymd("2002-09-01")

end_date <- ymd("2021-09-01")

# Define treatment start date

treatment_start_date <- ymd("2016-08-01")

# Define valid start and end periods

start_period <- interval(start_date, treatment_start_date)/months(1)

end_period <- interval(treatment_start_date, end_date)/months(1)

# Load the data -----------------------------------------------------------

# Load the interested parties province-month panel data

interested_parties_province_month <- readRDS("data/patents/processed/interested_parties_province_month.rds")

patents_main <- readRDS("data/patents/processed/patents_main.rds")

# Load the final dataset

df <- readRDS("data/full_dataset.rds")

# Patents -----------------------------------------------------------

## Patents filed per month ---------------------------------------------------------------------

# Grouping data of patents filed per month

patents_filed_per_month <-
  patents_main %>%
  group_by(filing_month_year) %>%
  summarise(n = n())  %>% 
  ungroup()  %>%
  arrange(desc(filing_month_year))

# Figure without date restriction

patents_filed_per_month_fig <-
  patents_filed_per_month %>% 
  ggplot(aes(x = filing_month_year, y = n)) +
  geom_line() +
  labs(x = "Filing date period",
       y = "Number of patents filed") +
  scale_x_date(date_breaks = "10 years",
               date_labels = "%Y") +
  scale_y_continuous(labels = comma, 
                     limits = c(0, 5000)) +
  theme_minimal() +
  labs(title = "Number of patents filed per month",
       subtitle = "All available periods",
       caption = "Note: Data obtained from the Canadian Intellectual Property Office (2023).") +
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

# Cumulative number of patents filed

patents_filed_per_month_cumulative <-
  patents_filed_per_month %>%
  arrange(filing_month_year) %>%
  mutate(cumulative = cumsum(n))

# Figure without date restriction with cumsum 

patents_filed_per_month_cumulative_fig <-
  patents_filed_per_month_cumulative %>% 
  ggplot(aes(x = filing_month_year, y = cumulative)) +
  geom_line() +
  labs(x = "Filing date period",
       y = "Cumulative number of patents filed") +
  scale_x_date(date_breaks = "10 years",
               date_labels = "%Y") +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  labs(title = "Cumulative number of patents filed per month",
       subtitle = "All available periods",
       caption = "Note: Data obtained from the Canadian Intellectual Property Office (2023).") +
  theme(text = element_text(size = 10, family = 'serif'),
        axis.text.x = element_text(angle = 90, hjust = 1),
        axis.line.x = element_line(colour = "black"),
        plot.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1, linetype = "solid"),
        plot.caption = element_text(hjust = 0),
        panel.grid.major = element_line(linetype = "dashed"),
        panel.grid.minor = element_line(linetype = "dashed"))

patents_filed_per_month_cumulative_fig

ggsave(filename = "figures/patents_filed_per_month_cumulative_fig.png", 
       plot = patents_filed_per_month_cumulative_fig,
       width = 17, 
       height = 10, 
       units = "cm",
       dpi = 800)

# Figure in the date range which is relevant

patents_filed_per_month_fig_relevant <-
  patents_filed_per_month %>%
  filter(filing_month_year %>% between(start_date, end_date)) %>%
    ggplot(aes(x = filing_month_year, y = n)) +
  geom_line() +
  labs(x = "Filing date period",
       y = "Number of patents filed") +
  scale_x_date(date_breaks = "2 years",
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

patents_filed_per_month_fig_relevant

ggsave(filename = "figures/patents_filed_per_month_fig_relevant.png", 
       plot = patents_filed_per_month_fig_relevant,
       width = 17, 
       height = 10, 
       units = "cm",
       dpi = 800)

# Cumulative number of patents filed in the relevant date range

patents_filed_per_month_cumulative_relevant <-
  patents_filed_per_month_cumulative %>%
  filter(filing_month_year %>% between(start_date, end_date))

patents_filed_per_month_cumulative_fig_relevant <-
       patents_filed_per_month_cumulative_relevant %>% 
       ggplot(aes(x = filing_month_year, y = cumulative)) +
       geom_line() +
       labs(x = "Filing date period",
        y = "Cumulative number of patents filed") +
       scale_x_date(date_breaks = "2 years",
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

patents_filed_per_month_cumulative_fig_relevant

# Interested parties -----------------------------------------------------------

## Interested parties per province and month ---------------------------------------------------------------------

# Plot the number of interested parties per province and month. Consider only largest Canadian provinces and all available periods

interested_parties_province_month_fig_all_dates <-
       interested_parties_province_month %>% 
       filter(province_code_clean %in% c("AB", "QC", "ON", "BC")) %>% 
       ggplot(aes(x = filing_month_year, y = interested_parties, group = province_code_clean, color = province_code_clean)) +
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
             legend.position = "bottom")

interested_parties_province_month_fig_all_dates

ggsave("figures/interested_parties_province_month_largest_provinces_all_dates.png", 
       plot = interested_parties_province_month_fig_all_dates, 
       width = 17, 
       height = 10, 
       units = "cm",
       dpi = 800)

# For the relevant date range

interested_parties_province_month_fig_relevant_dates <-
       interested_parties_province_month %>% 
       filter(filing_month_year %>% between(start_date, end_date),
              province_code_clean %in% c("AB", "QC", "ON", "BC")) %>% 
       ggplot(aes(x = filing_month_year, y = interested_parties, group = province_code_clean, color = province_code_clean)) +
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
             legend.position = "bottom")

interested_parties_province_month_fig_relevant_dates

ggsave("figures/interested_parties_province_month_largest_provinces_relevant_dates.png", 
       plot = interested_parties_province_month_fig_relevant_dates, 
       width = 17, 
       height = 10, 
       units = "cm",
       dpi = 800)

# The same, but with natural log of the number of interested parties

interested_parties_province_month_log_fig <-
       interested_parties_province_month %>% 
       filter(filing_month_year  %>% between(start_date, end_date),
              province_code_clean %in% c("AB", "QC", "ON", "BC")) %>%
       mutate(ln_interested_parties = log(interested_parties)) %>%
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
              legend.position = "bottom")

interested_parties_province_month_log_fig

ggsave("figures/interested_parties_province_month_largest_provinces_log.png", 
       plot = interested_parties_province_month_log_fig, 
       width = 17, 
       height = 10, 
       units = "cm",
       dpi = 800)

## Interested parties, common trends ---------------------------------------------------------------------

# Plot the number of interested parties in filed patents by treatment and control groups (AB vs sum of all others)

interested_parties_treatment_control_fig <-
       interested_parties_province_month %>% 
       filter(filing_month_year %>% between(start_date, end_date)) %>%
       mutate(treatment = ifelse(province_code_clean == "AB", "Treatment", "Control")) %>%
       group_by(filing_month_year, treatment) %>%
       summarise(patent_parties = sum(interested_parties, na.rm = T)) %>%
       ungroup() %>% 
       ggplot(aes(x = filing_month_year, y = log(patent_parties), group = treatment, color = treatment)) +
       geom_line() +
       scale_y_continuous(labels = comma) +
       scale_color_manual(values = c("Treatment" = "#0D3692", "Control" = "#E60F2D")) +
       labs(title = "Time series of the number of interested parties in filed patents",
            subtitle = "Natural log of the number of interested parties in filed patents",
            color = "Group",
            x = "Periods before AITC was passed",
            y = "Ln(Total Interested Parties)") +
       theme_minimal() +
       theme(text = element_text(size = 10, family = 'serif'),
             axis.text.x = element_text(angle = 90, hjust = 1),
             axis.line.x = element_line(colour = "black"),
             plot.background = element_rect(fill = "white", color = "white"),
             panel.border = element_rect(colour = "black", fill = NA, linewidth = 1, linetype = "solid"),
             plot.caption = element_text(hjust = 0),
             panel.grid.major = element_line(linetype = "dashed"),
             panel.grid.minor = element_line(linetype = "dashed"),
             legend.position.inside = c(0.92, 0.15))

interested_parties_treatment_control_fig


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

# Final dataset (df) data visualization -----------------------------------------------------------

## Interested parties, total treatment and control ----------------------------------------------------------------

# Plot the total number of interested parties in filed patents by treatment and control groups

interested_parties_total_trends_fig <-
       df %>% 
       filter(filing_month_year %>% between(start_date, end_date)) %>%
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
            y = "Ln(Total Interested Parties)") +
       theme_minimal() +
       theme(text = element_text(size = 10, family = 'serif'),
             axis.text.x = element_text(angle = 90, hjust = 1),
             axis.line.x = element_line(colour = "black"),
             plot.background = element_rect(fill = "white", color = "white"),
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
            y = "Ln(Inventors)") +
       theme_minimal() +
       theme(text = element_text(size = 10, family = 'serif'),
             axis.text.x = element_text(angle = 90, hjust = 1),
             axis.line.x = element_line(colour = "black"),
             plot.background = element_rect(fill = "white", color = "white"),
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

# Use the patchwork package to combine the two charts

joint_trends_figure <-
       interested_parties_total_trends_fig + 
       inventors_total_trends_fig + 
       plot_annotation(title = "Time series of the number of interested parties and inventors in filed patents",
                       caption = "Note: Data obtained from the Canadian Intellectual Property Office (2023).",
                       theme = theme(plot.title = element_text(family = "serif"),
                                     plot.caption = element_text(hjust = 0),
                                     text = element_text(family = "serif")))

ggsave("figures/joint_trends_figure.png",
       plot = joint_trends_figure,
       width = 25, 
       height = 10, 
       units = "cm",
       dpi = 800)

## Period numbers and years ----------------------------------------------------------------

# What period numbers are the most important years? I can are about 1980, 1990, 2000, 2010, 2016, 2020.
# Do a line plot which shows the date in the x-axis and in the labels I see the periods

df %>%
       filter(month_year %>% between(start_date, end_date)) %>%
       group_by(month_year, periods) %>%
       summarise(total_generation = sum(patents_filed)) %>% 
       ggplot(aes(x = month_year, y = total_generation)) +
       geom_line() +
       geom_text(data = . %>% filter(periods %in% c(-480, -450, -400, -360, -340, -240, -120, 0, 5, 30, 40, 50)), aes(label = periods), hjust = 0, vjust = 0, nudge_x = 10, nudge_y = 1000) +
       geom_vline(xintercept = treatment_starts, linetype = "dashed", color = "#56589e") +
       scale_x_date(date_labels = "%b%Y", date_breaks = "2 years") +
       labs(title = "Patents",
            subtitle = "Periods and dates",
            x = "Date",
            y = "Number of patents filed",
            caption = "Note: Data obtained from the Canadian Intellectual Property Office (2023).") +
       theme(axis.text.x = element_text(angle = 45, hjust = 1))