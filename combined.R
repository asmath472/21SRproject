library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(gridExtra)
movie_metadata <- read.csv("movies_metadata.csv", header = T)
tibble_metadata <- as_tibble(movie_metadata)

movie_data <- tibble_metadata %>%
  filter(!is.na(budget), !is.na(revenue)) %>%
  filter(budget > 0, revenue > 0) %>%
  select(id, budget, revenue)

movie_data$budget <- as.numeric(movie_data$budget)
movie_data$id <- as.numeric(movie_data$id)
movie_data$revenue <- as.numeric(movie_data$revenue)

# budget - ratings start 
ratings <- read.csv("ratings.csv", header = T)
tibble_ratings <- as_tibble(ratings)
average_ratings <- tibble_ratings %>%
  group_by(movieId) %>%
  summarise(average_rating = mean(rating))
names(movie_data)[names(movie_data) == "id"] <- "movieId"

joined <- inner_join(average_ratings, movie_data, by = "movieId")

joined_grouped <- joined %>%
  mutate(budget_group = ifelse(budget <= 1e+05, "5", 
                               ifelse(budget <= 1e+06, "6",
                                      ifelse(budget <= 1e+07, "7", 
                                             ifelse(budget <= 1e+08, "8", 
                                                    ifelse(budget <= 1e+09, "9", "over"))))))

budget_group_ratings <- joined_grouped %>%
  group_by(budget_group) %>%
  summarise(mean_rating = mean(average_rating))

joined_point <- joined %>%
  filter(budget < 3e+07) %>%
  group_by(budget) %>%
  summarise(average_rating = mean(average_rating))

#plot(joined_point$budget, joined_point$average_rating)
# budget - ratings end

# budget - revenue start
movie_data <- mutate(movie_data, earnings_rate = revenue/budget)

movie_data_grouped <- movie_data %>%
  filter(earnings_rate < 500) %>%
  mutate(budget_group = ifelse(budget <= 1e+05, "5", 
                               ifelse(budget <= 1e+06, "6",
                                      ifelse(budget <= 1e+07, "7", 
                                             ifelse(budget <= 1e+08, "8", 
                                                    ifelse(budget <= 1e+09, "9", "over"))))))

budget_group_earnings <-movie_data_grouped %>%
  group_by(budget_group) %>%
  summarise(mean_earnings = mean(earnings_rate))
#budget - revenue end

#genre-earnings_rate start
movie_data_genre <- tibble_metadata %>%
  filter(!is.na(genres), !is.na(budget), !is.na(revenue)) %>%
  filter(budget > 0, revenue > 0) %>%
  select(id, genres, budget, revenue)
movie_data_genre$budget <- as.numeric(movie_data_genre$budget)
movie_data_genre$revenue <- as.numeric(movie_data_genre$revenue)
movie_data_genre <- mutate(movie_data_genre, earnings_rate = revenue/budget)
d <- dim(movie_data_genre)
for (i in 1:d[1]) {
  movie_data_genre[i, ]$genres <- gsub(",|\\[|\\]|\\{","",movie_data_genre[i, ]$genres)
  movie_data_genre[i, ]$genres <- gsub(" ","",movie_data_genre[i, ]$genres)
  if (grepl("jpg", movie_data_genre[i, ]$genres))
    movie_data_genre[i, ]$genres <- NA
}
movie_data_genre <- movie_data_genre %>% filter(!is.na(genres))%>%separate(genres, into = c("genre1", "genre2", "genre3", "genre4", "genre5", "genre6", "genre7", "genre8"), sep = "}")
movie_data_genre
new_df <- data.frame(id = character(), genre = character(), earnings_rate = double())
str(new_df)
for (i in 1:d[1]) {
  if (is.na(movie_data_genre[i, ]$genre1) || (movie_data_genre[i, ]$genre1 == ""))
    next
  tmp <- data.frame(id = movie_data_genre[i, ]$id, genre = movie_data_genre[i, ]$genre1, earnings_rate = movie_data_genre[i, ]$earnings_rate)
  new_df <- rbind(new_df, tmp)
  if (is.na(movie_data_genre[i, ]$genre2) || (movie_data_genre[i, ]$genre2 == ""))
    next
  tmp <- data.frame(id = movie_data_genre[i, ]$id, genre = movie_data_genre[i, ]$genre2, earnings_rate = movie_data_genre[i, ]$earnings_rate)
  new_df <- rbind(new_df, tmp) 
  if (is.na(movie_data_genre[i, ]$genre3) || (movie_data_genre[i, ]$genre3 == ""))
    next
  tmp <- data.frame(id = movie_data_genre[i, ]$id, genre = movie_data_genre[i, ]$genre3, earnings_rate = movie_data_genre[i, ]$earnings_rate)
  new_df <- rbind(new_df, tmp)
  if (is.na(movie_data_genre[i, ]$genre4) || (movie_data_genre[i, ]$genre4 == ""))
    next
  tmp <- data.frame(id = movie_data_genre[i, ]$id, genre = movie_data_genre[i, ]$genre4, earnings_rate = movie_data_genre[i, ]$earnings_rate)
  new_df <- rbind(new_df, tmp)
  if (is.na(movie_data_genre[i, ]$genre5) || (movie_data_genre[i, ]$genre5 == ""))
    next
  tmp <- data.frame(id = movie_data_genre[i, ]$id, genre = movie_data_genre[i, ]$genre5, earnings_rate = movie_data_genre[i, ]$earnings_rate)
  new_df <- rbind(new_df, tmp)
  if (is.na(movie_data_genre[i, ]$genre6) || (movie_data_genre[i, ]$genre6 == ""))
    next
  tmp <- data.frame(id = movie_data_genre[i, ]$id, genre = movie_data_genre[i, ]$genre6, earnings_rate = movie_data_genre[i, ]$earnings_rate)
  new_df <- rbind(new_df, tmp)
  if (is.na(movie_data_genre[i, ]$genre7) || (movie_data_genre[i, ]$genre7 == ""))
    next
  tmp <- data.frame(id = movie_data_genre[i, ]$id, genre = movie_data_genre[i, ]$genre7, earnings_rate = movie_data_genre[i, ]$earnings_rate)
  new_df <- rbind(new_df, tmp)
}
seperated_by_genre <- as_tibble(new_df)
result <- seperated_by_genre %>%
  group_by(genre) %>%
  summarise(mean_earning = mean(earnings_rate))
#genre-earnings_rate end

p1 <- ggplot(data = budget_group_ratings, aes(x = budget_group, y = mean_rating)) + geom_col()
p2 <- ggplot(data = budget_group_earnings, aes(x = budget_group, y = mean_earnings)) + geom_col()
p3 <- ggplot(data = result, aes(x = genre, y = mean_earning)) + geom_col()
grid.arrange(arrangeGrob(p1, p2, ncol = 2, nrow = 1), arrangeGrob(p3, ncol = 1, nrow = 1))