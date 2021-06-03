library(tidyr)
library(dplyr)
library(stringr)
movie_metadata <- read.csv("movies_metadata.csv", header = T)
tibble_metadata <- as_tibble(movie_metadata)
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
ggplot(data = result, aes(x = genre, y = mean_earning)) + geom_col()