list.of.packages <- c("ggplot2", 
                      "dplyr", 
                      'tidyverse', 
                      'tidyr', 
                      'data.table', 
                      'reactable',
                      'shiny',
                      'htmltools',
                      'shinydashboard',
                      'shinythemes')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(ggplot2)
library(dplyr)
library(tidyverse)
library(tidyr)
library(reactable)
library(shiny)
library(htmltools)
library(shinydashboard)
library(shinythemes)
library(data.table)

pokemon_list <- read.csv('pokemon list.csv')
ball_list <- read.csv('poke ball list.csv')
critical_catch <- read.csv('critical catch.csv')
status_conditions <- read.csv('status conditions.csv')

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

# clean the data
mark1 <- dplyr::mutate(pokemon_list, Catch_Rate = as.numeric(gsub('\\*', '', Catch_Rate)))

# Catch Rates
catch_rate_values <- as.numeric(unique(mark1$Catch_Rate))
ball_values <- as.numeric(ball_list$Rate)
critical_values <- as.numeric(critical_catch$Multiplier)
status_values <- as.numeric(status_conditions$Condition)

# Combinations
full_df_combos <- expand.grid(catch_rate_values, ball_values, critical_values, status_values)
colnames(full_df_combos) <- c('Catch_Rate', 'Rate', 'Multiplier', 'Condition')

full_df_joins <- full_df_combos %>%
  inner_join(mark1) %>%
  inner_join(ball_list) %>%
  inner_join(critical_catch) %>%
  inner_join(status_conditions) %>%
  mutate(Total_Health = 200)