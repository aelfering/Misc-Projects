# Pokemon Catch Rate Probability
# Script by Alex Elfering
# Last Updated:
# Formula/Source:
#   https://bulbapedia.bulbagarden.net/wiki/Catch_rate#Modified_catch_rate_5
#   https://www.dragonflycave.com/mechanics/gen-vi-vii-capturing
#   https://bulbapedia.bulbagarden.net/wiki/List_of_Pok%C3%A9mon_by_catch_rate
#   Credit goes to Nintendo and GameFreak

#  Loading packages and initializing variables
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(reactable)
options(scipen = 999)

# These variables create the max HP and HP remaining by different percentages
hp_max <- 216
percent_health <- c(1.00, 0.75, 0.5, 0.25, 0.05)
hp_current <- hp_max * percent_health

# Stufful's catch rate is 140, and no special Pokeballs and status conditions used in this example
rate <- 140
bonus_ball <- 1
bonus_status <- 1

# Calculating the catch rate
X <- (( 3 * hp_max - 2 * hp_current ) * (rate * bonus_ball ) / (3 * hp_max) ) * bonus_status

# Calculating the critical catch rate
CC <- (X * 1)/6

# Shake probability
Y <- 65536/((255/X)^0.1875)

# What are the odds of a normal catch succeeding?
p_catch <- (1 - CC / 256) * (Y / 65536)^4

# What are the odds of a critical catch occuring?
p_crit_catch <- (CC / 256) * (Y / 65536)

init_summary <- data_frame(Health_Remaining = percent_health,
                           Probability_Ball = p_catch,
                           Probabilty_Critical = p_crit_catch,
                           #  The odds of a critical catch succeeding is the 4th root of p_catch 
                           Critical_Succeed = p_catch^0.25) %>%
  mutate(Probability_Ball = ifelse(Probability_Ball > 1, 1, Probability_Ball),
         Probabilty_Critical = ifelse(Probabilty_Critical > 1, 1, Probabilty_Critical),
         Critical_Succeed = ifelse(Critical_Succeed > 1, 1, Critical_Succeed),
         Critical_Succeed = ifelse(Probabilty_Critical == 0, 0, Critical_Succeed))

# What are the odds of catching a Pokemon within X number of balls?
balls <- seq(1, 1000, by = 1)

# This loop calculates the odds of catching a Pokemon within X number of balls by each group of HP remaining
balls_needed_list = list()
for(i in seq_along(p_catch)){
  
  odds <- 1-((1-p_catch[i])^balls)
  indices <- match(c(p_catch[i]), p_catch)
  
  balls_needed_df <- data_frame(odds = odds,
                                balls = balls,
                                iteration = indices)
  
  balls_needed_list[[i]] <- balls_needed_df
}

poke_ball_odds <- do.call(rbind, balls_needed_list) %>%
  filter(odds < 0.99)

# Visualizing odds of catching Stufful within X number of balls
ggplot(poke_ball_odds,
       aes(x = balls,
           y = odds,
           color = factor(iteration),
           group = iteration)) +
  geom_hline(yintercept = 0) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  scale_color_discrete(labels = c('100% Health', '75% Health', '50% Health', '25% Health', '5% Health')) +
  labs(title = 'Chances of Catching Stufful Within Total Throws',
       subtitle = 'Poke Ball throws are more effective the more damage you apply to the Pokemon',
       x = 'Total Poke Balls Thrown\n',
       y = 'Chances',
       color = 'Health Status',
       caption = 'Visualization by Alex Elfering\nFormula Courtesy of The Cave of Dragonflies & Bulbapedia') +
  scale_y_continuous(limits=c(0,1),
                     breaks = seq(0, 1, by = .1),
                     labels = scales::percent) +
  geom_label(aes(x = 2.3, y = 0.33, label = "The chance of of a successful capture\nin two throws increases the more damage applied."), 
             hjust = 0, vjust = 0.5, colour = "#555555", 
             fill = NA, label.size = NA, family="Arial", size = 4) +
  geom_curve(aes(x = 2.3, y = 0.33, xend = 2, yend = 0.44), 
             colour = "#555555", size=0.5, curvature = -0.2,
             arrow = arrow(length = unit(0.02, "npc"))) +
  geom_curve(aes(x = 2.5, y = 0.39, xend = 2.1, yend = 0.57), 
             colour = "#555555", size=0.5, curvature = 0.2,
             arrow = arrow(length = unit(0.02, "npc"))) +
  scale_x_continuous(breaks = seq(0, 15, by = 2)) +
  theme(plot.title = element_text(face = 'bold', size = 18, family = 'Arial'),
        plot.subtitle = element_text(size = 15, family = 'Arial'),
        plot.caption = element_text(size = 12, family = 'Arial'),
        axis.title = element_text(size = 12, family = 'Arial'),
        axis.text = element_text(size = 12, family = 'Arial'),
        legend.position = 'top',
        legend.box.background = element_rect(fill = "blue", colour = "transparent"),
        legend.key = element_rect(fill = "transparent", colour = "transparent"),
        strip.background = element_rect(fill = NA),
        panel.background = ggplot2::element_blank(),
        axis.line = element_line(colour = "#222222", linetype = "solid"),
        panel.grid.major.y = ggplot2::element_line(color = "#dedede", linetype = 'dashed'),
        panel.grid.major.x = ggplot2::element_line(color = "#dedede", linetype = 'dashed')) 
  
# Table visualizing odds of catching Stufful on the first try
# Functions courtesy of FiveThirtyEight:
#   https://glin.github.io/reactable/articles/womens-world-cup/womens-world-cup.html
odds_column <- function(maxWidth = 70, class = NULL, ...) {
  colDef(
    cell = format_pct,
    maxWidth = maxWidth,
    class = paste("cell number", class),
    style = function(value) {
      # Lighter color for <1%
      if (value < 0.01) {
        list(color = "#aaa")
      } else {
        list(color = "#aaa")
      }
    },
    ...
  )
}
format_pct <- function(value) {
  if (value == 0) "  \u2013 "    # en dash for 0%
  else if (value == 1) "\u2713"  # checkmark for 100%
  else if (value < 0.01) " <1%"
  else if (value > 0.99) ">99%"
  else formatC(paste0(round(value * 100, 2), "%"), width = 4)
}

init_table <- reactable(init_summary,
                        striped = TRUE,
                        highlight = TRUE,
                        bordered = TRUE,
                        resizable = TRUE,
                        style = list(fontFamily = 'Arial',
                                     fontSize = '14px'),
                        columns = list(
                          Health_Remaining = colDef(name = 'Health Remaining',
                                                    format = colFormat(percent = TRUE,
                                                                       digits = 0)),
                          Probability_Ball = odds_column(name = 'Chance of Success'),
                          Probabilty_Critical = odds_column(name = 'Chance of Critical Catch'),
                          Critical_Succeed = odds_column(name = 'Chance of Critical Catch Succeeding')
                          )
                        )

init_table
