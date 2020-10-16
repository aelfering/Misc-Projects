# Loading Packages
library(urbnmapr)
library(dplyr)
library(ggplot2)
library(colorspace)
library(tidyr)
library(tidyverse)

options(scipen = 999)

# Loading the Data
iowa_pop <- read.csv('PEPPOP2019.PEPANNRES_data_with_overlays_2020-09-13T091659.csv')

# Data Cleaning
# In this section, I:
#   Changed the column headers to something more relevant, and removed the first row
#   Removed ', County' from the county column to join with urbnmapr and filtered out 2010 population estimate
iowa_col_names <- c('id', 'county', 'estimate', 'population')

colnames(iowa_pop) <- iowa_col_names

iowa_filtered <- iowa_pop[-1,]

clean_iowa_df <- iowa_filtered %>%
  mutate(county = gsub(', Iowa', '', county)) %>%
  filter(!estimate %in% c('4/1/2010 population estimates base','7/1/2010 population estimate')) %>%
  mutate(population = as.numeric(population))

# Comparing the 2010 Census to the Most Recent Population Estimate in 2019

# 2010 Census Populations
init_county_pops <- clean_iowa_df %>%
  group_by(county) %>%
  mutate(survey = row_number()) %>%
  filter(survey == 1) %>%
  select(id,
         county,
         init_population = population)

# Most Recent Estimates
compare_since_census <- clean_iowa_df %>%
  inner_join(init_county_pops) %>%
  mutate(yoy = (population-init_population)/init_population,
         diff = population-init_population,
         date = as.numeric(substr(estimate, 5, 8))) %>%
  filter(date == max(date))

# returning iowa population value for comparison
iowa_growth <- compare_since_census %>%
  filter(county == 'Iowa') %>%
  select(yoy)

iowa_growth_int <- as.numeric(iowa_growth$yoy)

# The Map

# generating iowa state border 
iowa_state <- get_urbn_map("states", sf = TRUE) %>%
  filter(state_abbv == 'IA')
  
# generating iowa county borders and joining it to comparison df
iowa_counties <- get_urbn_map("counties", sf = TRUE) %>%
  filter(state_abbv == 'IA') %>%
  inner_join(compare_since_census, by = c('county_name' = 'county'))

iowa_counties %>% 
  ggplot(aes()) +
  geom_sf(aes(fill = yoy),
          color = 'white',
          size = 0.5) +
  geom_sf(data = subset(iowa_counties, yoy >= iowa_growth_int),
          fill = NA,
          color = 'black',
          size = 1) +
  geom_sf(data = iowa_state,
          color = 'black',
          fill = NA,
          size = 1) +
  scale_fill_gradientn(colours = c('#db4d0e', '#ed841d', '#fab64d', '#e7e7e7', '#9db8fa', '#5878e1', '#17439b'), 
                       labels = scales::percent,
                       values = scales::rescale(c(min(iowa_counties$yoy), 0, max(iowa_counties$yoy))),
                       guide = "colorbar", limits=c(min(iowa_counties$yoy),max(iowa_counties$yoy))) +
  labs(#title = 'Which Counties are Growing the Fastest in Iowa?',
       fill = 'Growth: ',
       caption = '\nVisualization by Alex Elfering\nSource: Census - ANNUAL ESTIMATES OF THE RESIDENT POPULATION'#,
       #subtitle = "In 2019, Iowa's population grew roughly 3.6% since the 2010 Census. The counties that grew faster than that rate\nare largely centered around the state's major cities in Des Moines, Cedar Rapids, Iowa City and the Quad Cities.\nHowever, Jefferson County is the only county with less than 20,000 people to grow faster than the state at 8.6%. \n"
       ) +
  guides(fill = guide_colorbar(title.vjust=1)) +
  theme(plot.title = element_text(face = 'bold', size = 18, family = 'Arial'),
        legend.position = 'top',
        legend.background = element_blank(),
        legend.key.width = unit(2, "cm"),
        legend.title.align = 0.5,
        #legend.key.height = unit(2, "cm"),
        plot.subtitle = element_text(size = 15, family = 'Arial'),
        plot.caption = element_text(size = 12, family = 'Arial'),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        strip.text = ggplot2::element_text(size = 12, hjust = 0, face = 'bold', color = 'black', family = 'Arial'),
        strip.background = element_rect(fill = NA),
        panel.background = ggplot2::element_blank(),
        axis.line = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank()) 





