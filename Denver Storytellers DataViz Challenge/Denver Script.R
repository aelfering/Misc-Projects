#   Denver Data Storytellers DataViz Challenge
#   Script by Alex Elfering
#   Published:  18 September 2020

#   This script compares total FEDs filed in 2020 vs 2019 for Denver

#### Load the Libraries and Data Sources ####
list.of.packages <- c("ggplot2", 
                      "dplyr", 
                      'tidyverse', 
                      'tidyr',
                      'tidylog',
                      'lubridate')

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(tidylog)
library(lubridate)

setwd("~/Documents/GitHub/Other-Projects/Denver Storytellers DataViz Challenge")

denver_filings <- read.csv('Denver FEDs Filed 20192020.csv')
denver_filings_19 <- read.csv('Denver FEDs Filed 2019.csv')

####  Cleaning 2019 Data  ####
denver_filings_19_dates <- dplyr::mutate(denver_filings_19, Date = as.Date(Filing.Date, "%m/%d/%y")) 

min_19_date <- min(denver_filings_19_dates$Date)
max_19_date <- max(denver_filings_19_dates$Date)

date_sequence_19 <- tibble(Date = seq.Date(min_19_date,
                                           max_19_date,
                                           by = 1))

cases_filed_ly <- denver_filings_19_dates %>%
  group_by(Date) %>%
  summarise(LY_Cases = n_distinct(Case.Number)) %>%
  ungroup() %>%
  full_join(date_sequence_19) %>%
  replace(is.na(.), 0) %>%
  arrange(Date) %>%
  mutate(rolling_n_ly = zoo::rollapplyr(LY_Cases, 7, sum, partial = TRUE))

####  Cleaning 2018 Data  ####
denver_clean <- denver_filings %>%
  mutate(Date = as.Date(Filing_Date, "%m/%d/%y"))

min_date <- min(denver_clean$Date)
max_date <- max(denver_clean$Date)

date_sequence <- tibble(Date = seq.Date(min_date,
                                        max_date,
                                        by = 1))

rolling_denver_cases <- denver_clean %>%
  group_by(Date) %>%
  summarise(n = n_distinct(Case_Number)) %>%
  ungroup() %>%
  full_join(date_sequence) %>%
  arrange(Date) %>%
  replace(is.na(.), 0) %>%
  mutate(rolling_n = zoo::rollapplyr(n, 7, sum, partial = TRUE),
         date_ly = Date-364) %>%
  full_join(cases_filed_ly, by = c('date_ly' = 'Date')) %>%
  filter(Date >= '2020-03-01',
         Date <= max_date)

####  The Visualization ####

ggplot(rolling_denver_cases,
       aes(x = Date,
           y = rolling_n)) +
  # LY Filings
  geom_line(size = 1,
            data = rolling_denver_cases,
            color = '#e7952f',
            mapping = aes(x = Date,
                          y = rolling_n_ly)) +
  # Event Annotations
  geom_vline(xintercept = as.Date('2020-03-16'),
             color = 'gray',
             size = 1,
             linetype = 'dashed') +
  geom_vline(xintercept = as.Date('2020-03-20'),
             color = 'gray',
             size = 1,
             linetype = 'dashed') +
  geom_vline(xintercept = as.Date('2020-04-30'),
             color = 'gray',
             size = 1,
             linetype = 'dashed') +
  geom_vline(xintercept = as.Date('2020-05-29'),
             color = 'gray',
             size = 1,
             linetype = 'dashed') +
  geom_vline(xintercept = as.Date('2020-06-13'),
             color = 'gray',
             size = 1,
             linetype = 'dashed') +
  geom_vline(xintercept = as.Date('2020-07-12'),
             color = 'gray',
             size = 1,
             linetype = 'dashed') +
  geom_vline(xintercept = as.Date('2020-08-10'),
             color = 'gray',
             size = 1,
             linetype = 'dashed') +
  geom_hline(yintercept = 0,
             color = '#484848') +
  # Rolling Cases Lines
  geom_line(size = 1,
            color = '#00429d') +
  # Labels
  # The extra spaces in the subtitle are for the annotations to be added in Illustrator
  labs(title = 'Evictions Filed in Denver have Slowly Climbed Since Mid-June',
       subtitle = 
         'After evictions climbed in March, Governor Polis issued several executive orders between March and mid-June suspending\nevictions. Since June 13, the focus shifted resuming evictions but increasing the notice landlords give their tenants to 30 days\nfrom 10 days. The result has been a gradual rise in evictions filed in Denver.
       
       
       
       ',
       x = '',
       y = '',
       caption = 'Visualization by Alex Elfering\nEvictions Data Source: City of Denver\nDate events confirmed via Denver Post, Denverite, Colorado Independent, City of Denver, Colorado Public Radio.') +
   #  Themes
   theme(plot.title = element_text(face = 'bold', 
                                  size = 18, 
                                  family = 'Helvetica'),
        plot.subtitle = element_text(size = 15, 
                                     family = 'Helvetica'),
        plot.caption = element_text(size = 10, 
                                    family = 'Helvetica'),
        axis.title.y = element_text(hjust = 0.5, 
                                    vjust = 0.5, 
                                    size = 12),
        axis.title.x = element_text(hjust = 0.5, 
                                    vjust = 0.5, 
                                    size = 12),
        axis.title = element_text(size = 12, 
                                  family = 'Helvetica'),
        axis.text = element_text(size = 12, 
                                 family = 'Helvetica',
                                 color = '#717171'),
        strip.background = element_rect(fill = NA),
        panel.background = ggplot2::element_blank(),
        axis.line = ggplot2::element_line(),
        panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_blank()) 

