library(tidyr)
library(dplyr)
library(ggplot2)
movie_metadata <- read.csv("movies_metadata.csv", header = T)
tibble_metadata <- as_tibble(movie_metadata)
movie_data <- tibble_metadata %>%
  filter(!is.na(budget), !is.na(revenue)) %>%
  filter(budget > 0, revenue > 0) %>%
  select(id, budget, revenue)
movie_data$budget <- as.numeric(movie_data$budget)
movie_data$id <- as.numeric(movie_data$id)

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

joined_filtered <- joined %>%
                    filter(budget < 1e+07)
plot(joined_filtered$budget, joined_filtered$average_rating)

joined_point <- joined %>%
                filter(budget < 3e+07) %>%
                group_by(budget) %>%
                summarise(average_rating = mean(average_rating))

plot(joined_point$budget, joined_point$average_rating)



