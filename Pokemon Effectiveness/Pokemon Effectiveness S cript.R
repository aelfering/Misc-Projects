list.of.packages <- c("ggplot2", 
                      "dplyr", 
                      'tidyverse', 
                      'tidyr',
                      'ggforce',
                      'grid',
                      'cowplot',
                      'gridExtra',
                      'tidylog')
setwd("~/Documents/GitHub/Other-Projects/Pokemon Effectiveness")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggforce)
library(grid)
library(gridExtra)
library(cowplot)
library(tidylog)

# What are the strength and weaknesses of dual-type pokemon?
# Using Pokemon from Sword & Shield
# Script by Alex Elfering
# Last Updated: [NEEDS A DATE]
# Data used:
#   https://www.pkmn.help/
#   https://www.ign.com/wikis/pokemon-sword-shield/List_of_Pokemon_(Pokedex)

# Data sources:
poke_types <- read.csv('pokemon types.csv')
pokemon_efficiency <- read.csv('efficiency_chart.csv')

####  Cleaning the Pokemon Types data frame  ####

pokemon_name_types <- poke_types %>%
  # Populated blank spaces with NA to fill them with Pokemon names and types
  mutate(Pokemon = ifelse(Pokemon == '', NA, Pokemon),
         Type = ifelse(Type == '', NA, Type)) %>%
  fill(Pokemon) %>%
  fill(Type) %>%
  distinct(Pokemon,
           Type) %>%
  # Need to pivot the types column into 2 columns for pokemon with dual-types
  group_by(Pokemon) %>%
  mutate(Key = row_number()) %>%
  spread(Key,
         Type) %>%
  ungroup() %>%
  unite(Type, 
        c('1', '2'), 
        na.rm = TRUE, 
        sep = '-')


# How many single-types of pokemon are there?
poke_types %>%
  # Populated blank spaces with NA to fill them with Pokemon names and types
  mutate(Pokemon = ifelse(Pokemon == '', NA, Pokemon),
         Type = ifelse(Type == '', NA, Type)) %>%
  fill(Pokemon) %>%
  fill(Type) %>%
  distinct(Pokemon,
           Type) %>%
  # Need to pivot the types column into 2 columns for pokemon with dual-types
  group_by(Pokemon) %>%
  mutate(Key = row_number()) %>%
  spread(Key,
         Type) %>%
  ungroup() %>%
  unite(Type, 
        c('1', '2'), 
        na.rm = TRUE, 
        sep = ' ') %>%
  mutate(Total = nchar(gsub('[^ ]+', '', Type)) + 1) %>%
  group_by(Total) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(N = sum(n),
         Pct = n/N) 



####  Effectiveness of Single-Type Pokemon ####

# single-type heat map
ggplot(pokemon_efficiency, 
       aes(x = Opponent, 
           y = Selection, 
           fill = factor(Damage))) + 
  geom_tile(size = 1,
            aes(width = 0.95, 
                height = 0.95)) +
  scale_fill_manual(values = c('#d90000', '#f39260', '#73a7c9', '#005377')) +
  geom_mark_rect(data = subset(pokemon_efficiency, Opponent == 'Steel'), 
                 aes(group = Opponent),
                 radius = 0,
                 expand = 0.029,
                 size = 0.75,
                 fill = NA) +
  geom_mark_rect(data = subset(pokemon_efficiency, Selection == 'Bug'), 
                 aes(group = Selection),
                 radius = 0,
                 expand = 0.029,
                 size = 0.75,
                 fill = NA) +
  labs(x = '\nDefending',
       y = '\nAttacking') +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 270, 
                                   vjust = 0.5, 
                                   hjust = 0, 
                                   size = 10),
        axis.title.y = element_text(angle = 270, 
                                    hjust = 0.5, 
                                    vjust = 0.5, 
                                    size = 12),
        axis.title.x = element_text(hjust = 0.5, 
                                    vjust = 0.5, 
                                    size = 12),
        axis.text = element_text(size = 10, 
                                 family = 'Arial'),
        strip.background = element_rect(fill = NA),
        panel.background = ggplot2::element_blank())  

need.legend <- ggplot(pokemon_efficiency,
                      aes(x = Opponent, 
                          y = Selection,
                          fill = factor(Damage))) +
  geom_tile(size = 1,
            aes(width = 0.95,
                height = 0.95)) +
  geom_mark_rect(data = subset(pokemon_efficiency, Opponent == 'Steel'), 
                 aes(group = Opponent),
                 radius = 0,
                 expand = 0.029,
                 size = .5,
                 fill = NA) +
  geom_mark_rect(data = subset(pokemon_efficiency, Selection == 'Bug'), 
                 aes(group = Selection),
                 radius = 0,
                 expand = 0.029,
                 size = .5,
                 fill = NA) +
  labs(x = '\nDefending',
       y = '\nAttacking',
       fill = 'How to Read: ') +
  scale_fill_manual(labels = c("No Effect", "0.5x Damage", "1x Damage", "2x Damage"),
                    values = c('#d90000', '#f39260', '#73a7c9', '#005377')) +
  theme(legend.position = 'top',
        axis.text.x = element_text(angle = 270, 
                                   vjust = 0.5, 
                                   hjust = 0, 
                                   size = 10),
        axis.title.y = element_text(angle = 270, 
                                    hjust = 0.5, 
                                    vjust = 0.5, 
                                    size = 12),
        axis.title.x = element_text(hjust = 0.5, 
                                    vjust = 0.5, 
                                    size = 12),
        axis.text = element_text(size = 10, 
                                 family = 'Arial'),
        strip.background = element_rect(fill = NA),
        panel.background = ggplot2::element_blank())  

poke.legend <- cowplot::get_legend(need.legend)

grid.newpage()
grid.draw(poke.legend)

####  Dual-Type Effectiveness ####

# How effective are different moves on dual-type pokemon?
opponent.dual.effectiveness <- pokemon_efficiency %>%
  full_join(pokemon_efficiency, 
            by = c('Selection' = 'Selection')) %>%
  mutate(Damage.y = ifelse(Opponent.x == Opponent.y, NA, Damage.y),
         Opponent.y = ifelse(Opponent.x == Opponent.y, NA, Opponent.y)) %>%
  mutate(Effectiveness = ifelse(is.na(Damage.y), Damage.x, Damage.x * Damage.y)) %>%
  unite(Opponent.Type, c('Opponent.x', 'Opponent.y'), sep = '-', na.rm = TRUE) %>%
  # include the pokemon from sword and shield
  inner_join(pokemon_name_types, by = c('Opponent.Type' = 'Type')) %>%
  select(Opponent.Type,
         Opponent.Name = Pokemon,
         Attack.Move = Selection,
         Effectiveness)

# These are the most effective and least effective moves you can make across all battles?

effectiveness_overall <- opponent.dual.effectiveness %>%
  distinct(Opponent.Type,
           Attack.Move,
           Effectiveness)

effectiveness_overall %>%
  group_by(Effectiveness) %>%
  summarise(Moves = n()) %>%
  ungroup() %>%
  mutate(Total = sum(Moves),
         Freq = Moves/Total) %>%
  ggplot(aes(x = factor(Effectiveness),
             y = Freq)) +
  geom_bar(stat = 'identity',
           position = 'identity') +
  geom_hline(yintercept = 0) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = 'How Effective are Attacks Overall in Sword & Shield',
       subtitle = 'Depending on the Pokemon selected, 50% of most moves will land 1x damage.',
       x = 'Damage Effectiveness') 
  



