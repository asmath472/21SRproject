library(tidyr)
library(dplyr)
library(ggplot2)
movie_metadata <- read.csv("movies_metadata.csv", header = T)
tibble_metadata <- as_tibble(movie_metadata)
movie_data <- tibble_metadata %>%
  filter(!is.na(budget), !is.na(revenue)) %>%
  filter(budget > 0, revenue > 0) %>%
  select(id, budget, revenue)
movie_data$budget <- as.numeric(movie_data_genre$budget)
movie_data$revenue <- as.numeric(movie_data_genre$revenue)
movie_data <- mutate(movie_data, earnings_rate = revenue/budget)
plot(movie_data$budget, movie_data$revenue)