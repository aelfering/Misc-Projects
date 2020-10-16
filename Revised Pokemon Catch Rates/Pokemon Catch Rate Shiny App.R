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

setwd("~/Desktop")

source('Pokemon Data Cleaning.R')

if (interactive()) {
  options(device.ask.default = FALSE)
  
  ui <- fluidPage(
    titlePanel("What are the Odds of Catching Pokemon Within 100 Throws?"),
    # The filters on the side panel
    sidebarLayout(
      sidebarPanel(
        selectInput("pokemon", "Select a Pokemon:",
                    sort(unique(full_df_joins$Pokemon_Name))),
        selectInput("ball", "Select a Ball:",
                    unique(full_df_joins$Ball)),
        selectInput("species", "How Many Pokemon have you Caught:",
                    factor(unique(full_df_joins$Species.Caught),levels=c("> 600", "451-600", "301-450", "151-300", "31-150", "≤ 30"))),
        sliderInput("range", "Percent of Health Remaining:",
                    min = 0.01, max = 1,
                    value = 1),
        sliderInput("variable", "Balls Thrown:",
                    min = 1, max = 100,
                    value = 1),
        h5("Table created by Alex Elfering"),
        width=2),
      mainPanel(reactableOutput(outputId = "table")))
  )
  
  
  # Server logic
  
  server <- function(input, output) {
    output$table <- renderReactable({
      
      # Creating the initial calculations that feed the probabilities for normal and critical catches
      init_calculations <- full_df_joins %>%
        mutate(HP_Remaining = round(Total_Health * input$range),
               Catch_Value = (( 3 * Total_Health - 2 * HP_Remaining ) * (Catch_Rate * Rate ) / (3 * Total_Health) ) * Condition,
               CC = (Catch_Value * Multiplier)/6,
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
      
      adj_filtered <- dplyr::filter(adj_calculations, Pokemon_Name == input$pokemon, Species.Caught == '≤ 30', Ball == input$ball)
      
      balls_int <- seq(1, 100) * 1.0
      
      ball_list_test <- list()
      for(i in seq_along(balls_int)){
        
        mark2 <- adj_filtered %>%
          group_by(Catch_Rate,
                   Pokemon_Name,
                   Rate,
                   Ball,
                   Status,
                   Condition) %>%
          mutate(Ball.Integer = balls_int[i]) %>%
          ungroup() %>%
          mutate
        
        ball_list_test[[i]] <- mark2
      }
      
      bind.test <- do.call(rbind, ball_list_test)
      
      odds_within_throws <- dplyr::mutate(bind.test, Odds = 1-(Prob_Not_Catching^Ball.Integer))
      
      target_pokemon_name <- unique(odds_within_throws$Pokemon_Name)
      
      tbl <- odds_within_throws %>%
        dplyr::select(Pokemon_Name,
                      Status,
                      Ball.Integer,
                      Odds) %>%
        mutate(Odds_Rounded = round(Odds, 2)) %>%
        filter(Ball.Integer == input$variable) %>%
        arrange(Odds_Rounded) %>%
        group_by(Status) %>%
        slice(which.min(Ball.Integer)) %>%
        ungroup() %>%
        arrange(Ball.Integer) %>%
        mutate(Odds_Statement = paste(round(Odds*100), ' out of 100', sep = '')) %>%
        dplyr::select(Pokemon = Pokemon_Name,
                      Balls.Thrown = Ball.Integer,
                      Status,
                      Odds,
                      Odds_Statement)
      
      
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
                pagination = FALSE,
                outlined = TRUE,
                highlight = TRUE,
                striped = TRUE,
                resizable = TRUE,
                wrap = TRUE,
                defaultColDef = colDef(headerClass = "header", 
                                       align = "left"),
                style = list(fontFamily = 'Arial', 
                             fontSize = '14px'),
                columns = list(
                  Odds = knockout_column(name = 'Odds of Catching'))
      )

    })
  }
  
  # Complete app with UI and server components
  shinyApp(ui, server)
}