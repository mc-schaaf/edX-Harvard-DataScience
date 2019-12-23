#####
##### Course 7: Linear Regression
#####

install.packages("Lahman")
library(tidyverse)
library(Lahman)
?Teams

Teams2 <- Teams %>% filter(yearID %in% c(1961:2001)) %>% mutate(
  R_per_game = R/G,
  AB_per_game = AB/G,
  W_per_game = W/G,
  E_per_game = E/G,
  X3B_per_game = X3B/G,
  X2B_per_game = X2B/G
) 
Teams2 %>% ggplot(aes(AB_per_game, R_per_game)) + geom_point(alpha = 0.5)
Teams2 %>% ggplot(aes(E_per_game, W_per_game)) + geom_point(alpha = 0.5)
Teams2 %>% ggplot(aes(X3B_per_game, X2B_per_game)) + geom_point(alpha = 0.5)
