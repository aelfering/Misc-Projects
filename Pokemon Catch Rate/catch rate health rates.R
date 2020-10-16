#   Loading the libraries
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(tidylog)

####    Variables for the catch rate!!!

#   What is the maximum health of the Pokemon?
hp_max <- 216

#   Testing various levels of health 
percent_health <- c(1.00, 0.25)
hp_current <- hp_max * percent_health

#   Creates: c(216, 162, 108, 54)

#   What is the pokemon's catch rate?
rate <- 140

#   Other variables such as the ball used and the status condition of the target pokemon
bonus_ball <- 1
bonus_status <- 1








catch_value <- (( 3 * hp_max - 2 * hp_current ) * (rate * 1 ) / (3 * hp_max) ) * bonus_status
catch_chance <- (65536 / (255/catch_value)^0.1875 )

biggerlist = list()
biggestlist = list()
biggerdf = data_frame()
mark1.dataframe = data_frame()
for(A in 1:500){
  for(N in 1:100){
    for(i in seq_along(catch_chance)){
      shakes <- sample(0:65535, 4)
      catches <- paste(catch_chance[i] >= shakes, collapse = " ")
  
      # This is what we want to get to
      correct.answer <- paste(c(TRUE, TRUE, TRUE, TRUE), collapse=" ")
  
      catch.test <- catches == correct.answer
      indices <- match(c(catch_chance[i]), catch_chance)
    
      biggerdf[i, 1] <- catch.test
      biggerdf[i, 2] <- indices
      biggerdf[i, 3] <- N
      }
  
    biggerlist[[N]] <- biggerdf
    }
  
  biggestlist[[A]] <- biggerlist
}

unbundle.list = list()
for(i in seq_along(biggestlist)){
  
  mark1 <- do.call(rbind, biggestlist[[i]])
  
  unbundle.list[[i]] <- mark1
  
  list.indices <- match(c(biggestlist[i]), biggestlist)
  
  testing <- Map(cbind, unbundle.list, iteration = list.indices)
}

testing



