library(dplyr)
library(ggplot2)
library(scales)
library(stringi)

options(scipen = 999)

border <- read.csv('Border_Crossing_Entry_Data.csv')

border$Value <- as.numeric(gsub(",", 
                                "", 
                                border$Value))

border$Date <- strptime(border$Date,
                        "%m/%d/%Y %H:%M:%S",
                        tz="GMT")
border$Date <- as.Date(border$Date)

recentDate <- max(border$Date)

# Return Month name and year number to group results easily 
border$MonthName <- as.factor(format(border$Date, "%B"))
border$YearNumber <- year(border$Date)

# Reorder factored month names chronologically
border$MonthName <- factor(border$MonthName,
                           levels = c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September',
                                      'October', 'November', 'December'))
border_sum <- border %>%
  filter(Measure %in% c('Truck Containers Empty', 'Truck Containers Full')) %>%
  mutate(PortNameState = paste(Port.Name, State, sep = ", ")) %>%
  group_by(MonthName,
           YearNumber,
           Measure) %>%
  summarise(Value = sum(Value)) %>%
  ungroup()

trucks_visual <- ggplot(border_sum, aes(x = YearNumber, y = Value)) +
  geom_line(aes(color = Measure, group = Measure), size = 1) +
  geom_point(aes(color = Measure, group = Measure), size = 2) +
  facet_wrap(~MonthName, ncol = 3) +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(limits = c(1995, 2020),
                     breaks = seq(1995, 2020, by = 5),
                     labels = c("1995", "2000", "2005", "2010", "2015", "2020")) +
  labs(title = "How many Truck Containers cross into the United States Month-Month",
         subtitle = "Adjusted to Examine Seasonality",
         x = "Year",
         y = "Total",
         caption = paste("Data through", recentDate, sep = " ")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 25),
        plot.subtitle = element_text(hjust = 0.5, size= 20),
        strip.text.x = element_text(size = 12),
        legend.position = "top")
  
  
  
  
  
  
  