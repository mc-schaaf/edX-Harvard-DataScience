#### Section 1 Assessment

MNF1 <- function(a, b, c)
{
  z <- ((-b + sqrt(b^2 - 4*a*c))/(2*a))
  return(z)
}

MNF2 <- function(a, b, c)
{
  z <- ((-b - sqrt(b^2 - 4*a*c))/(2*a))
  return(z)
}


MNF1(2, -1, -4)

MNF2(2, -1, -4)


log(1024, base = 4)

library(dslabs)
data(movielens)
str(movielens)

nrow(movielens)
ncol(movielens)

nlevels(movielens$genres)










###### Section 3 Assesment
library(dslabs)
library(dplyr)
data(heights)
options(digits = 3)    # report 3 significant digits for all answers

heights <- heights
avg <- mean(heights$height)
ind <- heights$height > avg
sum(ind)

ind2 <- ind & heights$sex == "Female"
sum(ind2)

mean(heights$sex == "Female")

min(heights$height)
match(min(heights$height), heights$height)
heights$sex[match(min(heights$height), heights$height)]

max(heights$height)

x <- 50:82
x2 <- x %in% heights$height
sum(!(x %in% heights$height))

heights2 <- heights %>% mutate(
  ht_cm = 2.54 * height
)

heights2$ht_cm[18]
mean(heights2$ht_cm)

females <- heights2 %>% filter(sex == "Female") 
nrow(females)
mean(females$ht_cm)


library(dslabs)
data(olive)
head(olive)

names(olive)
plot(olive$palmitic, olive$palmitoleic)
hist(olive$eicosenoic)
boxplot(olive$palmitic ~ olive$region)








library(dslabs)
library(tidyverse)
data(heights)

# basic QQ-plot
p <- heights %>% filter(sex == "Male") %>%
  ggplot(aes(sample = height))
p + geom_qq()

# QQ-plot against a normal distribution with same mean/sd as data
params <- heights %>%
  filter(sex == "Male") %>%
  summarize(mean = mean(height), sd = sd(height))
p + geom_qq(dparams = params) +
  geom_abline()

# QQ-plot of scaled data against the standard normal distribution

heights %>% ggplot(aes(sample = scale(height))) + geom_qq() + geom_abline()















####
#### CA1: 
####

rm(list = ls())

library(tidyverse)
library(dslabs)
data(heights)
options(digits = 3)


sum(ifelse(heights$sex == "Female", 1, 2))

mean(ifelse(heights$height > 72, heights$height, 0))

inches_to_ft <- function(inch){
  ft <- inch/12
  return(ft)
}

inches_to_ft(144)

sum(ifelse(inches_to_ft(heights$height) < 5, 1, 0))