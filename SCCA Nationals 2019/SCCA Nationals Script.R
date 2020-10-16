library(dplyr)
library(reshape2)

# SCCA Analysis
scca <- read.csv('2019-SCCA-Nationals-GStreet.csv')

# selecting the racers first and last time, and replacing DNF with 0
scca_times <- scca %>%
  select(Fname, 
         Lname, 
         Tires,
         Car.Make,
         Car.Model,
         Car.Year,
         Place,
         C1R1, 
         C1R2, 
         C1R3, 
         C2R1, 
         C2R2, 
         C2R3) %>%
  mutate(C1R2 = as.numeric(gsub("DNF", "0", C1R2)),
         C2R1 = as.numeric(gsub("DNF", "0", C2R1)),
         C2R2 = as.numeric(gsub("DNF", "0", C2R2)),
         Full.Name = paste(Fname, Lname, sep = " ")) %>%
  select(Full.Name, 
         Place,
         Tires,
         Car.Make,
         Car.Model,
         Car.Year,
         C1R1, 
         C1R2, 
         C1R3, 
         C2R1, 
         C2R2, 
         C2R3)

scca_times_melt <- melt(scca_times, id = c('Full.Name', 'Place', 'Tires', 'Car.Make', 'Car.Model', 'Car.Year'))

scca_melt_clean <- scca_times_melt %>%
  mutate(Course = substr(variable, start = 1, stop = 2),
         Run = substr(variable, start = 3, stop = 4)) %>%
  select(Full.Name,
         Place,
         Tires,
         Car.Make,
         Car.Model,
         Car.Year,
         Course,
         Run,
         Time = value)

scca_rank_times <- scca_melt_clean %>%
  filter(Time != 0) %>%
  group_by(Full.Name, Course) %>%
  mutate(Run.Rank = rank(Time)) %>%
  ungroup() %>%
  select(Full.Name,
         Course, 
         Run,
         Run.Rank)

final_scores <- scca_melt_clean %>%
  filter(Time != 0) %>%
  group_by(Course, Full.Name) %>%
  slice(which.min(Time)) %>%
  ungroup() %>%
  group_by(Full.Name) %>%
  summarise(Place = min(Place),
            Final.Score = sum(Time)) %>%
  arrange(Place) %>%
  as.data.frame()

first_course_scores <- scca_melt_clean %>%
  filter(Time != 0, Course == 'C1') %>%
  group_by(Course, Full.Name) %>%
  slice(which.min(Time)) %>%
  ungroup() %>%
  arrange(Time) %>%
  mutate(Run.Rank = order(Time)) %>%
  arrange(Run.Rank) %>%
  select(Full.Name,
         First.Course.Rank = Run.Rank,
         First.Course.Score = Time) %>%
  as.data.frame()

rank_compare_data_frame <- inner_join(final_scores, first_course_scores, by = c('Full.Name' = 'Full.Name'))
rank_compare_data_frame <- melt(rank_compare_data_frame)
rank_compare_data_frame <- inner_join(rank_compare_data_frame, final_scores, by = c('Full.Name' = 'Full.Name'))
rank_compare_data_frame <- inner_join(rank_compare_data_frame, first_course_scores, by = c('Full.Name' = 'Full.Name'))

scca_rank_full <- left_join(scca_melt_clean, 
                            scca_rank_times, 
                            by = c('Full.Name' = 'Full.Name',
                                   'Course' = 'Course',
                                   'Run' = 'Run'))

write.csv(scca_rank_full, 'Revised SCCA Race Times.csv')
write.csv(rank_compare_data_frame, 'CSSA Rank Comparison.csv')




