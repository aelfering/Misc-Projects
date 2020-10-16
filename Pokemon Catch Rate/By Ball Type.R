library(dplyr)
library(ggplot2)
library(tidyverse)
library(tidyr)
library(reactable)

# Testing different pokeballs and catch rates
catch_rates <- c(3, 10, 15, 25, 30, 35, 45, 50, 55, 60, 65, 70, 75, 80, 90, 100, 120, 125, 127, 130, 140, 145, 150, 155, 160, 170, 180, 190, 200, 205, 220, 225, 235, 255)
ball_multipliers <- c(1, 1.5, 2, 3.5, 5)
status_condition <- c(1, 1.5, 2, 2.5)
percent_health <- c(1.00, 0.75, 0.50, 0.25, 0.05)
pokedex_multiplier <- c(0, 0.5, 1, 1.5, 2, 2.5)
hp_max <- 216

variable_combos <- expand.grid(catch_rates, ball_multipliers, status_condition, percent_health, hp_max, pokedex_multiplier)

data_frame_names <- c('Catch_Rate', 'Ball_Multipliers', 'Status_Condition', 'Percent_Health', 'Max_HP', 'PokeDex_Multiplier')
colnames(variable_combos) <- data_frame_names

head(variable_combos)

# Creating the initial calculations that feed the probabilities for normal and critical catches
init_calculations <- variable_combos %>%
  mutate(HP_Remaining = round(Max_HP * Percent_Health),
         Catch_Value = (( 3 * Max_HP - 2 * HP_Remaining ) * (Catch_Rate * Ball_Multipliers ) / (3 * Max_HP) ) * Status_Condition,
         CC = (Catch_Value * PokeDex_Multiplier)/6,
         Shake_Prob = 65536/((255/Catch_Value)^0.1875),
         Crit_Catch_Prob = (CC / 256) * (Shake_Prob / 65536),
         Catch_Prob = (1 - CC / 256) * (Shake_Prob / 65536)^4,
         Crit_Catch_Succeed = Catch_Prob^0.25)

adj_calculations <- init_calculations %>%
  # if the catch value exceeds 255, then the catch automatically succeeds
  # if the critical catch probability = 0 then there is no chance of a critical catch succeeding
  mutate(Crit_Catch_Succeed = ifelse(Catch_Value > 255, NA, Crit_Catch_Succeed),
         Catch_Prob = ifelse(Catch_Value > 255, 1, Catch_Prob),
         Crit_Catch_Prob = ifelse(Crit_Catch_Prob > 1, 1, Crit_Catch_Prob),
         Crit_Catch_Succeed = ifelse(Crit_Catch_Prob == 0, 0, Crit_Catch_Succeed),
         Prob_Not_Catching = 1-Catch_Prob)

balls_int <- seq(1, 1000) * 1.0
  
ball_list_test <- list()
for(i in seq_along(balls_int)){
  
  mark2 <- adj_calculations %>%
    group_by(Catch_Rate,
             Ball_Multipliers,
             Status_Condition,
             Percent_Health,
             PokeDex_Multiplier) %>%
    mutate(Ball.Integer = balls_int[i]) %>%
    ungroup() %>%
    mutate
  
  ball_list_test[[i]] <- mark2
}

bind.test <- do.call(rbind, ball_list_test)

odds_within_throws <- dplyr::mutate(bind.test, Odds = 1-(Prob_Not_Catching^Ball.Integer))

head(odds_within_throws)

catch_rate_filter <- 10
status_condition_filter <- 1
percent_health_filter <- 0.75
pokedex_multiplier_filter <- 1

odds_filtered <- odds_within_throws %>%
  filter(Catch_Rate == catch_rate_filter,
         Status_Condition == status_condition_filter,
         Percent_Health == percent_health_filter,
         PokeDex_Multiplier == pokedex_multiplier_filter) #%>%
  #filter(Odds < 0.99)

tbl <- odds_filtered %>%
  filter(Ball.Integer == 1) %>%
  mutate(Crit_Catch_Succeed = ifelse(is.na(Crit_Catch_Succeed), 1, Crit_Catch_Succeed),
         Catch_Value = round(Catch_Value)) %>%
  select(Catch_Rate,
         Ball_Multipliers,
         Catch_Value,
         Catch_Prob,
         Crit_Catch_Prob,
         Crit_Catch_Succeed)

rating_column <- function(maxWidth = 55, ...) {
  colDef(maxWidth = maxWidth, align = "center", class = "cell number", ...)
}
group_column <- function(class = NULL, ...) {
  colDef(cell = format_pct, maxWidth = 70, align = "center", class = paste("cell number", class), ...)
}
knockout_column <- function(maxWidth = 70, class = NULL, ...) {
  colDef(
    cell = format_pct,
    maxWidth = maxWidth,
    class = paste("cell number", class),
    style = function(value) {
      # Lighter color for <1%
      if (value < 0.01) {
        list(color = "#aaa")
      } else {
        list(color = "#111", background = knockout_pct_color(value))
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
  else formatC(paste0(round(value * 100), "%"), width = 4)
}
make_color_pal <- function(colors, bias = 1) {
  get_color <- colorRamp(colors, bias = bias)
  function(x) rgb(get_color(x), maxColorValue = 255)
}

off_rating_color <- make_color_pal(c("#ff2700", "#f8fcf8", "#44ab43"), bias = 1.3)
def_rating_color <- make_color_pal(c("#ff2700", "#f8fcf8", "#44ab43"), bias = 0.6)
knockout_pct_color <- make_color_pal(c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"), bias = 2)

reactable(tbl,
          resizable = TRUE,
          style = list(fontFamily = 'Arial', 
                       fontSize = '14px'),
          columns = list(Catch_Rate = colDef(name = 'Catch Rate'),
                         Ball_Multipliers = colDef(name = 'Ball'),
                         Catch_Value = colDef(name = 'Catch Value'),
                         Catch_Prob = knockout_column(name = 'Catch Success'),
                         Crit_Catch_Prob = knockout_column(name = 'Critical Catch Occurs'),
                         Crit_Catch_Succeed = knockout_column(name = 'Critical Catch Succeeds')
                         ),
          )

ggplot(subset(odds_filtered, Odds < 0.99 & Ball_Multipliers %in% c(1, 1.5, 2, 3.5)),
       aes(x = Ball.Integer,
           y = Odds,
           group = Ball_Multipliers,
           color = factor(Ball_Multipliers))) +
  geom_line(size = 1) +
  #geom_point(size = 2) +
  scale_color_discrete(labels = c('Poke Ball', 'Ultra Ball', 'Great Ball', 'Dusk Ball')) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Which Poke Ball Gives You the Best Chance of Capture Over Time?",
       subtitle = 'Balls such as the Ultra Ball, Great Ball, and Dusk Ball increase your odds of catching the Pokemon with less balls compared to a normal Poke Ball.',
       color = 'Balls',
       caption = 'A catch value greater than 255 is an automatic capture.\nVisualization by Alex Elfering\nData Sources: Formula Courtesy of The Cave of Dragonflies; Bulbapedia, Serenbii, The Pokemon Company and Nintendo',
       y = 'Odds',
       x = 'Total Poke Ball Throws') +
  geom_hline(yintercept = 0,
             linetype = 'dashed') +
  theme(legend.position = 'top')
