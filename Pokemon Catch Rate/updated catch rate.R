hp_max <- 216
hp_current <- 216
rate <- 140
bonus_ball <- 1
bonus_status <- 1

####  Now calculating the catch value which impacts critical catch probability and overall catch probability
catch_value <- (( 3 * hp_max - 2 * hp_current ) * (rate * bonus_ball ) / (3 * hp_max) ) * bonus_status

####  data frames and lists for normal catch rate
datalist = data.frame()
biggerlist = list()

# if the catch value exceeds 255, then the Pokemon will be caught
if(catch_value >= 255){
  print("Pokemon is automatically caught. Congratulations!")

# If the value does not exceed 255, then a test to determine the chance of a critical catch occuring AND it's chance of succeeding
} else if (catch_value < 255) {
  
  # CRITICAL CATCH PROBABILITY
    crit_value <- floor((catch_value * 1)/6)
    crit_pa <- crit_value/256
  
  # CHANCE OF NORMAL CATCH SUCCEEDING
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
}

final.list <- do.call(rbind, biggerlist)

# What are the odds of a critical catch succeeding (if it happens)
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

# What is the success rate using one ball?
catch.rate.success <- iteration_sum %>%
  filter(`Balls Thrown` == 1) %>%
  select(Pct.Running)

# What is the success rate for a critical capture?
critical.catch.success <- (catch.rate.success$Pct.Running)^0.25





