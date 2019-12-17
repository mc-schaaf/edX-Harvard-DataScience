####
#### Free Asessment: Titanic
####


options(digits = 3)    # report 3 significant digits
library(tidyverse)
library(titanic)

titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))

plot1 <- ggplot(titanic, aes(x = Age, fill = Sex))
plot1 + geom_density(aes(), bw = 2, alpha = 0.1) + theme_bw()

params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))

plot1 <- ggplot(titanic, aes(sample = Age))
plot1 + geom_qq(dparams = params) + geom_qq_line(dparams = params)

plot1 <- titanic %>% ggplot(aes(x = Survived, fill = Sex)) + geom_bar(position = "dodge")
plot1

plot1 <- ggplot(titanic, aes(x = Age, fill = Survived))
plot1 + geom_density(aes(y = ..count..), bw = 2, alpha = 0.2) + theme_bw()

plot1 <- titanic %>% filter(Fare != 0 & !is.na(Fare)) %>% ggplot(aes(y = Fare, x = Survived))
plot1 + geom_boxplot() + theme_bw() + geom_point(alpha = 0.2, position = "jitter") + scale_y_continuous(trans = "log2")

plot1 <- titanic %>% ggplot(aes(x = Pclass, fill = Survived))
plot1 + geom_bar() + theme_bw()

plot1 <- ggplot(titanic, aes(x = Age, fill = Survived))
plot1 + geom_density(aes(y = ..count..), alpha = 0.3, position = "stack") + theme_bw() + facet_grid(Sex ~ Pclass)







####
#### CA 1: Harvard Computers
####

rm(list = ls())

library(tidyverse)
library(dslabs)
data(stars)
options(digits = 3)   # report 3 significant digits

stars <- as.data.frame(stars)
names(stars)

mean(stars$magnitude)
sd(stars$magnitude)

plot1 <- stars %>% ggplot(aes(x = magnitude))
plot1 + geom_density() ## bw nicht verändern!

plot1 <- stars %>% ggplot(aes(x = temp))
plot1 + geom_density(bw = 700) 

stars %>%
  ggplot(aes(log10(temp), magnitude)) +
  geom_point() +
  geom_text(aes(label = star), nudge_y = 1) + 
  scale_x_reverse() +
  scale_y_reverse() +
  geom_vline(xintercept = log10(5000)) + 
  theme_bw()



stars %>%
  ggplot(aes(log10(temp), magnitude, color = type)) +
  geom_point(size = 5) +
  scale_x_reverse() +
  scale_y_reverse() +
  geom_vline(xintercept = log10(5000)) + 
  theme_bw()



####
#### CA 2: Climate Change
####

rm(list = ls())

library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)

temp_carbon %>% filter(!is.na(carbon_emissions)) %>% .$year %>% min()
temp_carbon$carbon_emissions[temp_carbon$year %in% c(2014, 1751)]


temp_carbon %>% filter(!is.na(temp_anomaly)) %>% summarize(first = min(year), last = max(year), diff = -(temp_anomaly[year == min(year)]-temp_anomaly[year == max(year)]))

p <- temp_carbon %>% filter(!is.na(temp_anomaly)) %>% ggplot(aes(x = year, y = temp_anomaly))
p + geom_line() + geom_hline(aes(yintercept = 0), color = "blue") + 
  ylab("Temperature anomaly (degrees C)") + 
  ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018") +
  geom_text(aes(x = 2000, y = 0.05, label = "20th century mean"), color = "blue") +
  theme_bw() 

min(temp_carbon$year[temp_carbon$temp_anomaly > 0], na.rm = TRUE)
max(temp_carbon$year[temp_carbon$temp_anomaly < 0], na.rm = TRUE)
min(temp_carbon$year[temp_carbon$temp_anomaly > 0.5], na.rm = TRUE)


p <- temp_carbon %>% filter(!is.na(temp_anomaly)) %>% ggplot(aes(x = year))
p + geom_line(aes(y = temp_anomaly), color = "black") +
  geom_line(aes(y = land_anomaly), color = "brown") +
  geom_line(aes(y = ocean_anomaly), color = "blue") +
  geom_hline(aes(yintercept = 0), color = "blue") + 
  ylab("Temperature anomaly (degrees C)") + 
  ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018") +
  geom_text(aes(x = 2000, y = 0.05, label = "20th century mean"), color = "blue") +
  theme_bw() 



greenhouse_gases %>%
  ggplot(aes(x = year, y = concentration)) +
  geom_line() +
  facet_grid( gas ~. , scales = "free") +
  geom_vline(aes(xintercept = 1850)) +
  ylab("Concentration (ch4/n2o ppb, co2 ppm)") +
  ggtitle("Atmospheric greenhouse gas concentration by year, 0-2000") + 
  theme_bw()



p <- temp_carbon %>% filter(!is.na(carbon_emissions)) %>% ggplot(aes(x = year))
p + geom_line(aes(y = carbon_emissions), color = "black") +
  theme_bw() 


temp_carbon$carbon_emissions[temp_carbon$year == 2014] / temp_carbon$carbon_emissions[temp_carbon$year == 1980]

co2_time <- historic_co2 %>% ggplot(aes(x = year, y = co2, color = source)) + geom_line() + theme_bw()
co2_time
co2_time + xlim(c(-800000, -775000))
co2_time + xlim(c(-375000, -330000))
co2_time + xlim(c(-3000, 2018))





