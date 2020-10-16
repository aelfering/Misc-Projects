# Instagram Likes by Time of Day and the Hour

# How to transcribe your Instagram Like and Comments data into actionable data that you can visualize?
# created by Alex Elfering
# 23 May 2020

# Instagram and Facebook Activity Report
# How often am I active on social media? (can I control for scrolling time?)

library(jsonlite)
library(lubridate)
library(dplyr)
library(tidyr)
library(tidyverse)
library(scales)
library(reshape2)
library(data.table)
library(directlabels)
library(RcppRoll)
library(zoo)
library(anytime)

setwd("~/Desktop/Social Media Data/Instagram")

#### Date Parameters ####
floor_date <- floor_date(Sys.Date(), unit = 'months')-1
floor_month <- month(floor_date)
floor_year <- year(floor_date)
floor_day <- day(floor_date)

#### Access your files based on the folder they reside and consolidate them ####
instagram.likes <- fromJSON('likes.json', flatten = TRUE)
instagram.comments <- fromJSON('comments.json', flatten = TRUE)

insta.likes <- as.data.frame(instagram.likes[[1]])
insta.comments <- as.data.frame(instagram.likes[[2]])
post.comments <- as.data.frame(instagram.comments[[1]])

#### Create uniform dataframes to bind rows by ####
insta.post.comments <- post.comments %>%
  mutate(V1 = gsub('T', ' ', V1)) %>%
  mutate(V1 = gsub('\\+00:00', '', V1)) %>%
  mutate(V1 = ymd_hms(V1),
         Interaction= 'Post.Comments') %>%
  mutate(Date = as.Date(V1, "%m/%d/%Y"),
         Year = year(V1),
         Month  = month(V1),
         Day = day(V1),
         Hour = hour(V1),
         Minute = minute(V1)) %>%
  select(Date.Time = V1,
         Account = V3,
         Interaction,
         Date,
         Year,
         Month,
         Day,
         Hour,
         Minute)

insta.time.likes <- insta.likes %>%
  mutate(V1 = gsub('T', ' ', V1)) %>%
  mutate(V1 = gsub('\\+00:00', '', V1)) %>%
  mutate(V1 = ymd_hms(V1),
         Interaction= 'Likes') %>%
  mutate(Date = as.Date(V1, "%m/%d/%Y"),
         Year = year(V1),
         Month  = month(V1),
         Day = day(V1),
         Hour = hour(V1),
         Minute = minute(V1)) %>%
  select(Date.Time = V1,
         Account = V2,
         Interaction,
         Date,
         Year,
         Month,
         Day,
         Hour,
         Minute)

insta.time.comments <- insta.comments %>%
  mutate(V1 = gsub('T', ' ', V1)) %>%
  mutate(V1 = gsub('\\+00:00', '', V1)) %>%
  mutate(V1 = ymd_hms(V1),
         Interaction= 'Comments') %>%
  mutate(Date = as.Date(V1, "%m/%d/%Y"),
         Year = year(V1),
         Month  = month(V1),
         Day = day(V1),
         Hour = hour(V1),
         Minute = minute(V1)) %>%
  select(Date.Time = V1,
         Account = V2,
         Interaction,
         Date,
         Year,
         Month,
         Day,
         Hour,
         Minute)

instagram.like.activity <- bind_rows(insta.time.likes,
                                     insta.time.comments,
                                     insta.post.comments)

#### Likes by the Hour & Time of Day ####
hourly.likes <- instagram.like.activity %>%
  mutate(Year = year(Date)) %>%
  mutate(month = floor_date(Date, unit = 'month')) %>%
  group_by(Year,
           month,
           Hour) %>%
  summarise(Likes = n()) %>%
  ungroup() %>%
  group_by(Year,
           month) %>%
  complete(Hour = seq(0, 23, by = 1)) %>%
  replace(is.na(.), 0) %>%
  mutate(YEALY_LIKES = sum(Likes)) %>%
  ungroup() %>%
  mutate(Time.Day = ifelse(Hour >= 5 & Hour < 11, 'Morning (5a-10a)',
                           ifelse(Hour >= 11 & Hour < 17, 'Afternoon (11a-4p)',
                                  ifelse(Hour >= 17 & Hour < 23, 'Evening (5p-10p)',
                                         ifelse(Hour == 23 | Hour < 5, 'Night (11p-4a)', 'Need Time')))))

time.day.likes <- hourly.likes %>%
  filter(month >= '2013-01-01') %>%
  group_by(month,
           Time.Day) %>%
  summarise(Likes = sum(Likes)) %>%
  ungroup() %>%
  group_by(month) %>%
  mutate(Yearly.Likes = sum(Likes)) %>%
  ungroup() %>%
  group_by(Time.Day) %>%
  mutate(Rolling.Time.Likes = rollapplyr(Likes, 12, sum, partial = TRUE),
         Rolling.Yearly.Likes = rollapplyr(Yearly.Likes, 12, sum, partial = TRUE)) %>%
  mutate(Pct.Freq = Rolling.Time.Likes/Rolling.Yearly.Likes)

str(time.day.likes)

time.day.likes$Time.Day <- factor(time.day.likes$Time.Day, 
                                  levels = c('Morning (5a-10a)', 
                                             'Afternoon (11a-4p)', 
                                             'Evening (5p-10p)', 
                                             'Night (11p-4a)'))

ggplot(time.day.likes,
       aes(x = month,
           y = Pct.Freq,
           group = Time.Day,
           color = Time.Day)) +
  geom_line(size = 1) +
  geom_vline(xintercept = as.numeric(as.Date('2013-05-01')),
             linetype = 'dashed',
             color = '#646464') +
  geom_vline(xintercept = as.numeric(as.Date('2013-08-01')),
             linetype = 'dashed',
             color = '#646464') +
  geom_vline(xintercept = as.numeric(as.Date('2017-05-01')),
             linetype = 'dashed',
             color = '#646464') +
  geom_vline(xintercept = as.numeric(as.Date('2019-01-01')),
             linetype = 'dashed',
             color = '#646464') +
  geom_vline(xintercept = as.numeric(as.Date('2020-03-01')),
             linetype = 'dashed',
             color = '#646464') +
  scale_y_continuous(labels = scales::percent) +
  labs(title = 'Rolling 12-Month Proportion of Instagram Likes By Time of Day',
       subtitle = 'Between January 2013 and April 2020',
       x = 'Month',
       y = 'Percent of Activity',
       color = 'Time of Day') +
  theme(plot.title = element_text(face = 'bold', size = 18, family = 'Arial'),
        legend.position = 'top',
        legend.background=element_blank(),
        plot.subtitle = element_text(size = 15, family = 'Arial'),
        plot.caption = element_text(size = 12, family = 'Arial'),
        axis.title = element_text(size = 12, family = 'Arial'),
        axis.text = element_text(size = 12, family = 'Arial'),
        strip.text = ggplot2::element_text(size = 12, hjust = 0, face = 'bold', color = 'brown', family = 'Arial'),
        strip.background = element_rect(fill = NA),
        panel.background = ggplot2::element_blank(),
        axis.line = element_line(colour = "#222222", linetype = "solid"),
        panel.grid.major.y = element_line(colour = "gray", linetype = "dashed"),
        panel.grid.major.x = element_blank()) 

insta.hour <- instagram.like.activity %>%
  mutate(Year = year(Date)) %>%
  filter(month(Date) <= month(floor_date)) %>%
  group_by(Year,
           Hour) %>%
  summarise(Likes = n()) %>%
  ungroup() %>%
  group_by(Year) %>%
  complete(Hour = seq(0, 23, by = 1)) %>%
  replace(is.na(.), 0) %>%
  mutate(YEALY_LIKES = sum(Likes)) %>%
  ungroup() %>%
  mutate(Pct.Freq = Likes/YEALY_LIKES,
         Time.Day = ifelse(Hour >= 5 & Hour < 11, 'Morning',
                           ifelse(Hour >= 11 & Hour < 17, 'Afternoon',
                                  ifelse(Hour >= 17 & Hour < 23, 'Evening',
                                         ifelse(Hour == 23 | Hour < 5, 'Night', 'Need Time')))))

cy.hour <- subset(insta.hour, Year == max(Year))
py.hour <- subset(insta.hour, Year == max(Year)-4)

hour.join <- inner_join(cy.hour,
                        py.hour,
                        by = c('Hour' = 'Hour'))

ggplot(hour.join,
       aes(x = Hour,
           y = Pct.Freq.x)) +
  geom_bar(stat = 'identity',
           position = 'identity',
           aes(fill = Pct.Freq.x > Pct.Freq.y)) +
  geom_bar(stat = 'identity',
           position = 'identity',
           data = hour.join,
           fill = NA,
           color = 'black',
           mapping = aes(x = Hour,
                         y = Pct.Freq.y)) +
  scale_y_continuous(labels = scales::percent) +
  geom_hline(yintercept = 0) +
  labs(title = 'Comparing 2016 Instagram Activity to 2020 Instagram Activity',
       x = 'Hour of Day',
       y = 'Percent of Instagram Activity',
       subtitle = 'Between January and April of 2016 and 2020, respectively.',
       fill = 'Did 2020 Activity Exceed 2016?') +
  scale_fill_manual(values = c('gray', '#f69864')) +
  theme(plot.title = element_text(face = 'bold', size = 18, family = 'Arial'),
        legend.position = 'top',
        legend.background=element_blank(),
        plot.subtitle = element_text(size = 15, family = 'Arial'),
        plot.caption = element_text(size = 12, family = 'Arial'),
        axis.title = element_text(size = 12, family = 'Arial'),
        axis.text = element_text(size = 12, family = 'Arial'),
        strip.text = ggplot2::element_text(size = 12, hjust = 0, face = 'bold', color = 'black', family = 'Arial'),
        strip.background = element_rect(fill = NA),
        panel.background = ggplot2::element_blank(),
        axis.line = element_line(colour = "#222222", linetype = "solid"),
        panel.grid.major.y = element_line(colour = "gray", linetype = "dashed"),
        panel.grid.major.x = element_blank()) 

mark1 <- instagram.like.activity %>%
  mutate(Year = year(Date),
         WeekDay = weekdays.Date(Date)) %>%
  filter(month(Date) <= month(floor_date)) %>%
  group_by(Year,
           WeekDay,
           Hour) %>%
  summarise(Likes = n()) %>%
  ungroup() %>%
  group_by(Year,
           WeekDay) %>%
  complete(Hour = seq(0, 23, by = 1)) %>%
  replace(is.na(.), 0) %>%
  mutate(YEALY_LIKES = sum(Likes)) %>%
  ungroup() %>%
  mutate(Pct.Freq = Likes/YEALY_LIKES,
         Time.Day = ifelse(Hour >= 5 & Hour < 11, 'Morning',
                           ifelse(Hour >= 11 & Hour < 17, 'Afternoon',
                                  ifelse(Hour >= 17 & Hour < 23, 'Evening',
                                         ifelse(Hour == 23 | Hour < 5, 'Night', 'Need Time')))))

mark.cy <- subset(mark1, Year == max(Year))
mark.py <- subset(mark1, Year == max(Year)-4)

mark.join <- inner_join(mark.cy,
                        mark.py,
                        by = c('Hour' = 'Hour',
                               'WeekDay' = 'WeekDay'))

mark.join$WeekDay <- factor(mark.join$WeekDay, 
                            levels = c('Monday', 
                                       'Tuesday', 
                                       'Wednesday', 
                                       'Thursday', 
                                       'Friday', 
                                       'Saturday', 
                                       'Sunday'))

ggplot(mark.join,
       aes(x = Hour,
           y = Pct.Freq.x)) +
  geom_bar(stat = 'identity',
           position = 'identity',
           aes(fill = Pct.Freq.x > Pct.Freq.y)) +
  geom_bar(stat = 'identity',
           position = 'identity',
           data = mark.join,
           fill = NA,
           color = 'black',
           mapping = aes(x = Hour,
                         y = Pct.Freq.y)) +
  scale_y_continuous(labels = scales::percent) +
  geom_hline(yintercept = 0) +
  facet_wrap(~WeekDay, 
             ncol = 4) +
  labs(title = 'Comparing 2016 Instagram Activity to 2020 Instagram Activity by Week Day',
       x = 'Hour of Day',
       y = 'Percent of Instagram Activity',
       subtitle = 'Between January and April of 2016 and 2020, respectively.',
       fill = 'Did 2020 Activity Exceed 2016?') +
  scale_fill_manual(values = c('gray', '#f69864')) +
  theme(plot.title = element_text(face = 'bold', size = 18, family = 'Arial'),
        legend.position = 'top',
        legend.background=element_blank(),
        plot.subtitle = element_text(size = 15, family = 'Arial'),
        plot.caption = element_text(size = 12, family = 'Arial'),
        axis.title = element_text(size = 12, family = 'Arial'),
        axis.text = element_text(size = 12, family = 'Arial'),
        strip.text = ggplot2::element_text(size = 12, hjust = 0, face = 'bold', color = 'black', family = 'Arial'),
        strip.background = element_rect(fill = NA),
        panel.background = ggplot2::element_blank(),
        axis.line = element_line(colour = "#222222", linetype = "solid"),
        panel.grid.major.y = element_line(colour = "gray", linetype = "dashed"),
        panel.grid.major.x = element_blank()) 

time.weekday.likes <- instagram.like.activity %>%
  mutate(Year = year(Date),
         Month = floor_date(Date, unit = 'months'),
         WeekDay = weekdays.Date(Date)) %>%
  filter(Date <= floor_date,
         Year > 2012) %>%
  group_by(Year,
           Month,
           WeekDay,
           Hour) %>%
  summarise(Likes = n()) %>%
  ungroup() %>%
  group_by(Year,
           Month) %>%
  complete(WeekDay = weekdays(Sys.Date()+0:6),
           Hour = seq(0, 23, by = 1)) %>%
  replace(is.na(.), 0) %>%
  mutate(YEAR_MONTH_LIKES = sum(Likes)) %>%
  ungroup() %>%
  mutate(Time.Day = ifelse(Hour >= 5 & Hour < 11, 'Morning (5a-10a)',
                           ifelse(Hour >= 11 & Hour < 17, 'Afternoon (11a-4p)',
                                  ifelse(Hour >= 17 & Hour < 23, 'Evening (5p-10p)',
                                         ifelse(Hour == 23 | Hour < 5, 'Night (11p-4a)', 'Need Time'))))) %>%
  group_by(Year,
           Month,
           WeekDay,
           Time.Day) %>%
  summarise(Likes = sum(Likes)) %>%
  ungroup() %>%
  group_by(Year,
           Month,
           WeekDay) %>%
  mutate(Week.Day.Likes = sum(Likes)) %>%
  ungroup() %>%
  group_by(WeekDay,
           Time.Day) %>%
  mutate(Rolling.WeekDay = rollapplyr(Week.Day.Likes, 12, sum, partial = TRUE),
         Rolling.Time = rollapplyr(Likes, 12, sum, partial = TRUE),
         Roll.Freq = Rolling.Time/Rolling.WeekDay)

time.weekday.likes$WeekDay <- factor(time.weekday.likes$WeekDay, 
                                     levels = c('Monday', 
                                                'Tuesday', 
                                                'Wednesday', 
                                                'Thursday', 
                                                'Friday', 
                                                'Saturday', 
                                                'Sunday'))

time.weekday.likes$Time.Day <- factor(time.weekday.likes$Time.Day, 
                                      levels = c('Morning (5a-10a)',
                                                 'Afternoon (11a-4p)',
                                                 'Evening (5p-10p)',
                                                 'Night (11p-4a)'))

ggplot(time.weekday.likes,
       aes(x = Month,
           y = Roll.Freq,
           group = Time.Day,
           color = Time.Day)) +
  geom_hline(yintercept = 0) +
  geom_line(size = 1) +
  facet_wrap(~WeekDay,
             ncol = 4) +
  scale_y_continuous(labels = scales::percent) +
  geom_vline(xintercept = as.numeric(as.Date('2013-05-01')),
             linetype = 'dashed',
             color = '#646464') +
  geom_vline(xintercept = as.numeric(as.Date('2013-08-01')),
             linetype = 'dashed',
             color = '#646464') +
  geom_vline(xintercept = as.numeric(as.Date('2017-05-01')),
             linetype = 'dashed',
             color = '#646464') +
  geom_vline(xintercept = as.numeric(as.Date('2019-01-01')),
             linetype = 'dashed',
             color = '#646464') +
  geom_vline(xintercept = as.numeric(as.Date('2020-03-01')),
             linetype = 'dashed',
             color = '#646464') +
  labs(title = 'Rolling 12-Month Proportion of Instagram Likes By Time of Day & Week Day',
       subtitle = 'Between January 2013 and April 2020',
       x = 'Month',
       y = 'Percent of Activity',
       color = 'Time of Day') +
  theme(plot.title = element_text(face = 'bold', size = 18, family = 'Arial'),
        legend.position = 'top',
        plot.subtitle = element_text(size = 15, family = 'Arial'),
        plot.caption = element_text(size = 12, family = 'Arial'),
        axis.title = element_text(size = 12, family = 'Arial'),
        axis.text = element_text(size = 12, family = 'Arial'),
        strip.text = ggplot2::element_text(size = 12, hjust = 0, face = 'bold', color = 'black', family = 'Arial'),
        strip.background = element_rect(fill = NA),
        panel.background = ggplot2::element_blank(),
        axis.line = element_line(colour = "#222222", linetype = "solid"),
        panel.grid.major.y = element_line(colour = "gray", linetype = "dashed"),
        panel.grid.major.x = element_blank()) 
