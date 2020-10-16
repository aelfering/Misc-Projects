# Star Wars
# Source: FiveThirtyEight and Survey Monkey Audience
# Code by: Alex Elfering

list.of.packages <- c("dplyr", "ggplot2", "reshape2")
lapply(list.of.packages, require, character.only = TRUE)

starwars <- read.csv('StarWars.csv')
starwars.yes <- subset(starwars, starwars$Have.you.seen.any.of.the.6.films.in.the.Star.Wars.franchise. != 'No')

####  Which respondents have seen all six films? ####
starwars.movies.seen <- starwars.yes %>%
  select(RespondentID,
         Which.of.the.following.Star.Wars.films.have.you.seen..Please.select.all.that.apply.,
         X,
         X.1,
         X.2, 
         X.3,
         X.4)

starwars.movies.seen <- starwars.movies.seen[-1,]
colnames(starwars.movies.seen) <- c("RespondentID", 
                                    "Phantom Menace",
                                    "Attack of the Clones",
                                    "Revenge of the Sith",
                                    "A New Hope",
                                    "Empire Strikes Back",
                                    "Return of the Jedi")

movies.seen.pivoted <- melt(starwars.movies.seen, id = c("RespondentID"))

seen.6.movies <- movies.seen.pivoted %>%
  select(RespondentID,
         Movie = variable,
         Seen = value) %>%
  mutate(Seen.Value = ifelse(Seen == "", 0, 1)) %>%
  group_by(RespondentID) %>%
  summarise(All.Movies.Seen = sum(Seen.Value)) %>%
  ungroup() %>%
  filter(All.Movies.Seen == 6) %>%
  select(RespondentID)

####  Which Characters have the Highest and Lowest Favorability Ratings? ####
characters.data.frame <- starwars.yes %>%
  select(RespondentID,
         `Han Solo` = Please.state.whether.you.view.the.following.characters.favorably..unfavorably..or.are.unfamiliar.with.him.her.,
         `Luke Skywalker` = X.10,
         `Leia Organa` = X.11,
         `Anakin Skywalker` = X.12,
         `Obi-Wan Kenobi` = X.13,
         `Emperor Palpatine` = X.14,
         `Darth Vader` = X.15,
         `Lando Calrissian` = X.16,
         `Boba Fett` = X.17,
         `C-3P0` = X.18,
         `R2-D2` = X.19,
         `Jar Jar Binks` = X.20,
         `Padme Amidala` = X.21,
         `Yoda` = X.22)

clean.characters.data.frame <- characters.data.frame[-1,]

charac.pivot <- melt(clean.characters.data.frame, id = c('RespondentID'))

charac.pivot$Character <- charac.pivot$variable
charac.pivot$Favorability <- as.factor(charac.pivot$value)
charac.pivot$Favorability <- factor(charac.pivot$Favorability, levels = c("Very unfavorably",
                                                                          "Somewhat unfavorably",
                                                                          "Neither favorably nor unfavorably (neutral)",
                                                                          "Somewhat favorably",
                                                                          "Very favorably",
                                                                          "Unfamiliar (N/A)",
                                                                          ""))

charac.pivot.all.six.films <- inner_join(charac.pivot, seen.6.movies, by = c('RespondentID' = 'RespondentID')) #adjust for fans who have seen all six movies
charac.no.null <- charac.pivot.all.six.films %>%
  filter(Favorability != "") %>%
  mutate(Count = 1)

total.votes <- charac.no.null %>%
  group_by(Character) %>%
  summarise(Total.Ct = sum(Count))

fav.votes <- charac.no.null %>%
  group_by(Character, Favorability) %>%
  summarise(Fav.Ct = sum(Count))

character.fav.votes <- inner_join(fav.votes, total.votes, by = c("Character" = "Character"))

charac.percent <- character.fav.votes %>%
  mutate(Pct = Fav.Ct/Total.Ct)

ggplot(data = charac.percent, 
       aes(x = Character,
           y = Pct,
           fill = Favorability)) +
  geom_bar(stat = "identity", 
           position = "fill") +
  coord_flip() +
  scale_fill_manual(values = c("#FF9500",     # dark orange
                               "#FFBC5E",     # light orange 
                               "#DCDCDC",     # light gray
                               "#7EB8FF",     # light blue
                               "#4E89D2",     # dark blue
                               "#898989" )) + # dark gray
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_minimal() +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        plot.subtitle = element_text(size = 17),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_blank()) +
  scale_y_continuous(labels = scales::percent_format()) +
  guides(fill = guide_legend(nrow = 1, reverse = TRUE, title = "Favorability Rating"),
         colour = guide_legend(nrow = 1)) +
  labs(title = "Which Star Wars Characters have the Highest Favorability Rating?",
       subtitle = "Many fans who have seen all six movies strongly view Jar Jar Binks, Emperor Palpatine, and Darth Vader the most\nunfavorably whereas Yoda, Obi-Wan Kenobi, Han Solo, Luke Skywalker and Leia Organa are strongly viewed favorably.",
       y = 'Percent Favorability',
       caption = "\nVisual by Alex Elfering\nSource: SurveyMonkey Audience and FiveThirtyEight\n471 respondents who saw all movies were asked to rank each Star Wars character by their favorability. Survey was conducted between June 3-6, 2014.")

####  Which Movies Received the Highest Ranks? ####
movie.ranks <- starwars.yes %>%
  select(RespondentID,
         `The Phantom Menace` = Please.rank.the.Star.Wars.films.in.order.of.preference.with.1.being.your.favorite.film.in.the.franchise.and.6.being.your.least.favorite.film.,
         `The Attack of the Clones` = X.5,
         `The Revenge of the Sith` = X.6,
         `A New Hope` = X.7,
         `The Empire Strikes Back` = X.8,
         `The Return of the Jedi` = X.9)

movie.ranks <- movie.ranks[-1,]

movie.pivot <- melt(movie.ranks, id = c('RespondentID'))
movie.pivot <- movie.pivot %>%
  select(RespondentID,
         Film = variable,
         Rank = value)

movie.pivot.seen.all.six <- inner_join(movie.pivot, seen.6.movies, by = c("RespondentID" = "RespondentID")) # Adjust for fans who have seen all six films

clean.movie.ranks <- movie.pivot.seen.all.six %>%
  filter(Rank != "") %>%
  group_by(Film, Rank) %>%
  summarise(ct = n()) %>%
  ungroup() %>%
  as.data.frame()

clean.movie.ranks$Rank <- as.factor(clean.movie.ranks$Rank)
clean.movie.ranks$Rank <- factor(clean.movie.ranks$Rank, levels = c("6", "5", "4", "3", "2", "1"))

ggplot(data = clean.movie.ranks, aes(x= Film, y = ct, fill = Rank)) +
  geom_bar(stat = "identity", 
           position = "fill") +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        plot.subtitle = element_text(size = 17),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_manual(values = c("#A95C00",     # 6
                               "#FF991F",     # 5 
                               "#FFBA67",     # 4
                               "#6CC5FF",     # 3
                               "#1FA6FF",     # 2
                               "#0065A7" )) + # 1
  scale_y_continuous(labels = scales::percent_format()) +
  guides(fill = guide_legend(nrow = 1, reverse = TRUE, title = "Rank"),
         colour = guide_legend(nrow = 1)) +
  labs(x = 'Movie',
       y = 'Percent of Rank',
       title = "The Empire Strikes Back is Highly Ranked Among other Star Wars Films",
       subtitle = "Over 3 out of 4 respondents who have seen all six Star Wars films rank Episode V a 3 or above as their favorite\nmovie followed by Episode VI and Episode IV. Episodes I, II, and III, however, are ranked less favorably with 3 out\nof 4 fans ranking Episode II a 4 or lower.",
       caption = "\nVisual by Alex Elfering\nSource: SurveyMonkey Audience and FiveThirtyEight\n471 respondents who saw all movies were asked to rank from 1 to 6 (1 being the highest) each Star Wars film. Survey was conducted between June 3-6, 2014.")




