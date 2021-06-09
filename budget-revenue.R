
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
movie_data$revenue <- as.numeric(movie_data$revenue)
movie_data <- mutate(movie_data, earnings_rate = revenue/budget) 
movie_data_filtered <- movie_data %>%
                filter(earnings_rate < 500)
movie_data_point <- movie_data %>%
              group_by(budget) %>%
              summarise(average_earnings_rate = mean(earnings_rate)) %>%
              filter(average_earnings_rate < 500, budget <5e+07)

f1 <- formula(movie_data_filtered$earnings_rate~movie_data_filtered$budget)
f2 <- formula(movie_data_filtered$earnings_rate~movie_data_filtered$budget + I(movie_data_filtered$budget^2))
f3 <- formula(movie_data_filtered$earnings_rate~movie_data_filtered$budget + I(movie_data_filtered$budget^2) + I(movie_data_filtered$budget^3))

summary(lm(formula = f1))
summary(lm(formula = f2))
summary(lm(formula = f3))

ggplot(movie_data_filtered, aes(budget,earnings_rate)) + geom_point() + coord_cartesian(ylim = c(0, 50)) + stat_smooth(method = "lm", formula = y ~ poly(x, 1), size = 1)



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


ggplot(data = budget_group_earnings, aes(x = budget_group, y = mean_earnings)) + geom_col() 


# plot(movie_data_point$budget, movie_data_point$average_earnings_rate)