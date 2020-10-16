library(tidyr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(tidylog)

####  Setting Pokemon stats that affect the catch rate
hp_max <- 216
hp_current <- 216
rate <- 140
bonus_ball <- 1
bonus_status <- 1

####  Now calculating the catch value which impacts critical catch probability and overall catch probability
catch_value <- (( 3 * hp_max - 2 * hp_current ) * (rate * 1 ) / (3 * hp_max) ) * bonus_status

#### What are the odds of a critical capture happening and succeeding?
crit_list <- list()
crit_pa <- list()
for (i in 1:1000){
  
  # Generate a random number between 0-255
  crit_catch_num <- sample(0:255, 1)
  # Calculate the critical catch value
  crit_value <- floor((catch_value * 1)/6)
  
  # If the critical catch value is greater than the random number, the critical catch will happen
  # If it does happen, that number is divided by 255 to calculate probability
  crit_test <- crit_value > crit_catch_num
  crit_variable <- ifelse(crit_test == TRUE, crit_value/255, NA)
  
  # Returning output of TRUE/FALSE test in list
  crit_list[i] <- crit_test
  
  # Return probability in a list
  if(crit_value > crit_catch_num){
    crit_pa[i] <- crit_variable
  }
}


data.frame(do.call(rbind, crit_list)) %>%
  group_by(do.call.rbind..crit_list.) %>%
  summarise(N = n()) %>%
  ungroup() %>%
  mutate(Total = sum(N),
         Pct = N/Total)


####  What are the odds of a normal capture happening and succeeding?
datalist = data.frame()
biggerlist = list()

# This loop repeats the same battle 1,000 times to estimate the average total Poke balls needed 
for (N in 1:1000) {
  
  # This loop calculates how many Poke balls it would take to capture the opposing Pokemon
  for (i in 1:1000) {
    
    # When you cast a Poke Ball, the ball shakes four times
    # If the numbers from the sample are smaller than the catch_value, then the Trainer catches the Pokemon
    shakes <- sample(0:65535, 4)
    
    catches <- paste((65536 / (255/catch_value)^0.1875 ) >= shakes, collapse = " ")
    
    # This is what we want to get to 
    correct.answer <- paste(c(TRUE, TRUE, TRUE, TRUE), collapse=" ")
    
    caught.pokemon <- catches == correct.answer
    
    datalist[i, 1] <- caught.pokemon 
    datalist[i, 2] <- N
  }
  
  biggerlist[[N]] <- datalist
}

final.list <- do.call(rbind, biggerlist)

####  Summarizing the findings of normal capture
captures_iteration <- final.list %>%
  group_by(V2) %>%
  mutate(Rows = row_number()) %>%
  filter(V1 == TRUE) %>%
  slice(which.min(V2))

iteration_sum <- captures_iteration %>%
  group_by(Rows) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(Total = sum(n),
         Running.N = cumsum(n),
         Pct.Running = Running.N/Total) %>%
  select(`Balls Thrown` = Rows,
         Tries = n,
         Running.Tries = Running.N,
         Pct.Running) 


  # Visualization
  ggplot(iteration_sum,
         aes(x = `Balls Thrown`,
             y = Pct.Running)) +
  geom_vline(xintercept = 5,
             linetype = 'dashed') +
  geom_hline(yintercept = .5,
             linetype = 'dashed') +
  geom_hline(yintercept = 0.75,
             linetype = 'dashed') +
    geom_hline(yintercept = 0,
               linetype = 'dashed') +
  geom_hline(yintercept = 0.95,
             linetype = 'dashed') +
  geom_line(color = 'orange',
            size = 1) +
  geom_point(color = 'orange',
             size = 3) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = 'What are the Odds of Catching a Pokemon?',
       subtitle = 'Based on total balls thrown in battle.',
       x = 'Consecutive Balls Thrown',
       y = 'Chances of Catching a Pokemon',
       caption = 'Visualization by Alex Elfering\nSource: Bulbapedia')

