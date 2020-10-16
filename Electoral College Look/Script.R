library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)

####  Electoral College State Totals  ####
ec_totals <- read.csv('electoral vote totals.csv')

ec_clean <- ec_totals %>%
  mutate(D = as.numeric(gsub('\\-', '', D)),
         R = as.numeric(gsub('\\-', '', R)),
         State = gsub('\\*', '', State))

ec_pivot <- ec_clean %>%
  group_by(Year, 
           State) %>%
  pivot_longer(cols = c('D', 'R'),
               names_to = 'Party') %>%
  mutate(Party = ifelse(Party == 'D', 'democrat', Party),
         Party = ifelse(Party == 'R', 'republican', Party)) %>%
  filter(!is.na(value)) 

pop_state_vote <- data_frame(Year = c(2000, 2004, 2008, 2012, 2016, 2000, 2004, 2008, 2012, 2016),
                             State = c('Nebraska', 'Nebraska', 'Nebraska', 'Nebraska', 'Nebraska', 'Maine', 'Maine', 'Maine', 'Maine', 'Maine'),
                             Total.Votes = c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2),
                             Party = c('republican', 'republican', 'republican', 'republican', 'republican', 'democrat', 'democrat', 'democrat', 'democrat', 'democrat'),
                             value = c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2))

cd_state_vote <- data_frame(Year = c(2000, 2004, 2008, 2012, 2016, 2000, 2004, 2008, 2012, 2016, 2000, 2004, 2008, 2012, 2016, 2000, 2004, 2008, 2012, 2016, 2000, 2004, 2008, 2012, 2016),
                            State = c('NE-1', 'NE-1', 'NE-1', 'NE-1', 'NE-1', 'NE-2', 'NE-2', 'NE-2', 'NE-2', 'NE-2', 'NE-3', 'NE-3', 'NE-3', 'NE-3', 'NE-3', 'ME-1', 'ME-1', 'ME-1', 'ME-1', 'ME-1', 'ME-2', 'ME-2', 'ME-2', 'ME-2', 'ME-2'),
                            Total.Votes = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
                            Party = c('republican', 'republican', 'republican', 'republican', 'republican',
                                      'republican', 'republican', 'democrat', 'republican', 'republican',
                                      'republican', 'republican', 'republican', 'republican', 'republican',
                                      'democrat', 'democrat', 'democrat', 'democrat', 'democrat',
                                      'democrat', 'democrat', 'democrat', 'democrat', 'republican'),
                            value = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))

exclude_states <- dplyr::distinct(pop_state_vote, Year, State)

pop_state <- ec_pivot %>%
  anti_join(exclude_states) %>%
  bind_rows(pop_state_vote) %>%
  bind_rows(cd_state_vote) %>%
  dplyr::select(year = Year,
                state = State,
                total.votes = Total.Votes,
                party = Party,
                value)

####  NE/ME Congressional District Margins  ####
elections_00_04_08 <- read.csv('elections 2000, 2004, 2008.csv')
elections_12 <- read.csv('elections 2012.csv')
elections_16 <- read.csv('elections 2016.csv')

pivot_election_00_04_08 <- elections_00_04_08 %>%
  filter(State %in% c('NE', 'ME')) %>%
  mutate(Margin_00 = Gore-Bush.I,
         Margin_04 = Kerry-Bush.II,
         Margin_08 = Obama-McCain) %>%
  unite(CD, c('State', 'District'), sep = '-') %>%
  dplyr::select(CD,
                Margin_00,
                Margin_04,
                Margin_08) %>%
  group_by(CD) %>%
  pivot_longer(cols = -c('CD'),
               names_to = 'Year') %>%
  mutate(Year = ifelse(Year == 'Margin_00', 2000, Year),
         Year = ifelse(Year == 'Margin_04', 2004, Year),
         Year = ifelse(Year == 'Margin_08', 2008, Year),
         Year = as.numeric(Year),
         value = value/100) %>%
  dplyr::select(state = CD,
                year = Year,
                margin = value) %>%
  as.data.frame()

pivot_election_12 <- elections_12 %>%
  filter(grepl('NE', CD) | grepl('ME', CD)) %>%
  mutate(Margin = Obama.2012-Romney.2012,
         Margin = Margin/100,
         Year = 2012) %>%
  dplyr::select(state = CD, year = Year, margin = Margin)

pivot_election_16 <- elections_16 %>%
  filter(grepl('NE', CD) | grepl('ME', CD)) %>%
  mutate(Margin = Clinton-Trump,
         Margin = Margin/100,
         Year = 2016) %>%
  dplyr::select(state = CD, year = Year, margin = Margin)

cd_ne_me_margins <- bind_rows(pivot_election_00_04_08, pivot_election_12, pivot_election_16)

cd_ne_me_clean <- cd_ne_me_margins %>%
  mutate(winner = ifelse(margin > 0, 'democrat', 'republican'),
         total.votes = 1,
         party = winner,
         value = 1)

####  State Vote Percentages  ####
state_vote_totals <- read.csv('1976-2016-president-2.csv')

head(state_vote_totals)

state_pct_df <- state_vote_totals %>%
  group_by(year,
           state,
           party) %>%
  summarise(votes = sum(candidatevotes)) %>%
  ungroup() %>%
  group_by(year,
           state) %>%
  mutate(state_votes = sum(votes)) %>%
  ungroup() %>%
  mutate(pct = votes/state_votes)

dem_state_votes <- dplyr::filter(state_pct_df, party %in% c('democrat', 'democratic-farmer-labor')) %>%
  dplyr::select(year, state, democrat = pct)

rep_state_votes <- dplyr::filter(state_pct_df, party == 'republican') %>%
  dplyr::select(year, state, republican = pct)

vote_compare <- inner_join(dem_state_votes, rep_state_votes)
margin_compare <- dplyr::mutate(vote_compare, margin = democrat - republican)

margin_party_win <- dplyr::mutate(margin_compare, winner = ifelse(democrat > republican, 'democrat', 'republican')) %>%
  dplyr::mutate(state1 = state) %>%
  dplyr::select(year, 
                state,
                state1,
                margin,
                winner)

ggplot(margin_party_win,
       aes(x = year,
           y = margin,
           group = state)) +
  geom_line(data = transform(margin_party_win,
                             state = NULL), 
            aes(group = state1), 
            size = 1,
            color = 'gray',
            alpha = 0.2) +   
  geom_hline(yintercept = 0,
             linetype = 'dashed') +
  geom_line(aes(group = state), 
            color = 'black',
            size = 1) +  
  scale_y_continuous(labels = scales::percent) +
  facet_geo(~state) + 
  labs(title = 'History of Presidential Elections by State 1976-2016',
       caption = 'Visualization by Alex Elfering\nSource: MIT Election Lab',
       x = '',
       y = '') +
  theme(plot.title = element_text(face = 'bold', size = 18, family = 'Arial'),
        plot.subtitle = element_text(size = 15, family = 'Arial'),
        plot.caption = element_text(size = 12, family = 'Arial'),
        axis.title = element_text(size = 12, family = 'Arial'),
        axis.text = element_text(size = 12, family = 'Arial'),
        strip.text = ggplot2::element_text(size = 12, hjust = 0, face = 'bold', color = 'black', family = 'Arial'),
        strip.background = element_rect(fill = NA),
        panel.background = ggplot2::element_blank(),
        axis.line = element_line(colour = "#222222", linetype = "solid"),
        panel.grid.major.y = ggplot2::element_line(color = "#dedede", linetype = 'dashed'),
        panel.grid.major.x = element_blank()) 














