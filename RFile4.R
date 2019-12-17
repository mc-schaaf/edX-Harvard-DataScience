rm(list = ls())
library(tidyverse)
library(dslabs)
options(digits = 3)

a1 <- 0.2
b1 <- 0.6
c1 <- 0.15
d1 <- 0.05
notfound <- 0.1

Pr_notfound1 <- ((1 - b1) + (b1*notfound))
Pr_B_notfound <- (notfound*b1)/Pr_notfound1
Pr_B_notfound

Pr_A_notfound <- a1/Pr_notfound1
Pr_A_notfound

Pr_C_notfound <- c1/Pr_notfound1
Pr_C_notfound



Pr_found1 <- 1-Pr_notfound1
Pr_found1

Pr_found2 <- Pr_A_notfound * (1-notfound) * (1-Pr_found1)
Pr_found2

Pr_found12 <- Pr_found1 + Pr_found2
Pr_found12



##
## CA: BREXIT
##


# suggested libraries and options
library(tidyverse)
options(digits = 3)

# load brexit_polls object
library(dslabs)
data(brexit_polls)
brexit_polls <- brexit_polls

true_p <- 0.481    # official proportion voting "Remain"
true_d <- 2*true_p-1    # official spread

## Q1
N <- 1500
N* true_p
sqrt(true_p*(1-true_p))/sqrt(N)*N
true_p
sqrt(true_p*(1-true_p))/sqrt(N)
true_d
sqrt(true_p*(1-true_p))/sqrt(N)*2

## Q2
brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread/2 + 1/2))

mean(brexit_polls$spread)
sd(brexit_polls$spread)
mean(brexit_polls$x_hat)
sd(brexit_polls$x_hat)

## Q3
dat <- brexit_polls[1,]
ci <- dat$x_hat + c(qnorm(0.025), qnorm(0.975)) * (sqrt(dat$x_hat*(1-dat$x_hat))/sqrt(dat$samplesize))
ci

## Q4
june_polls <- brexit_polls %>% filter(enddate >= "2016-06-01") %>% 
  mutate(se_x_hat = sqrt(x_hat*(1-x_hat)) / sqrt(samplesize)) %>%
  mutate(se_spread = 2*se_x_hat) %>%
  mutate(lower = spread + qnorm(0.025)*se_spread, upper = spread + qnorm(0.975)*se_spread) %>%
  mutate(hit = ifelse(lower <= true_d & upper >= true_d, TRUE, FALSE))

june_polls %>% summarize(n = n(), incl0 = mean(ifelse(lower <= 0 & upper >= 0, TRUE, FALSE)), rem0 = mean(lower > 0), hitrate = mean(hit))
june_polls %>% group_by(pollster) %>% summarize(n = n(), hitrate = mean(hit)) %>% arrange(desc(hitrate))
june_polls %>% group_by(poll_type) %>% ggplot(aes(x = poll_type, y = spread)) + geom_boxplot()


## Q7
combined_by_type <- june_polls %>%
  group_by(poll_type) %>%
  summarize(N = sum(samplesize),
            spread = sum(spread*samplesize)/N,
            p_hat = (spread + 1)/2) %>%
  mutate(se_x_hat = sqrt(p_hat*(1-p_hat)) / sqrt(N)) %>%
  mutate(se_spread = 2*se_x_hat) %>%
  mutate(lower = spread + qnorm(0.025)*se_spread, upper = spread + qnorm(0.975)*se_spread)

combined_by_type$lower[combined_by_type$poll_type == "Online"]
combined_by_type$upper[combined_by_type$poll_type == "Online"]

combined_by_type$lower - combined_by_type$upper


## Q9
brexit_hit <- brexit_polls %>%
  mutate(p_hat = (spread + 1)/2,
         se_spread = 2*sqrt(p_hat*(1-p_hat)/samplesize),
         spread_lower = spread - qnorm(.975)*se_spread,
         spread_upper = spread + qnorm(.975)*se_spread,
         hit = spread_lower < -0.038 & spread_upper > -0.038) %>%
  select(poll_type, hit)

dat <- brexit_hit %>% group_by(poll_type, hit) %>% summarize(n = n()) %>% spread(poll_type, n)
dat %>% select(-hit) %>% chisq.test()

(dat$Online[2] / sum(dat$Online)) / (dat$Online[1] / sum(dat$Online))
(dat$Telephone[2] / sum(dat$Telephone)) / (dat$Telephone[1] / sum(dat$Telephone))

((dat$Online[2] / sum(dat$Online)) / (dat$Online[1] / sum(dat$Online))) / ((dat$Telephone[2] / sum(dat$Telephone)) / (dat$Telephone[1] / sum(dat$Telephone)))


## Q11
brexit_polls %>% ggplot(aes(x = enddate, y = spread, color = poll_type)) + 
  geom_smooth(method = "loess", span = 0.4) + geom_point() + geom_hline(aes(yintercept =true_d))

## Q12
brexit_long <- brexit_polls %>%
  gather(vote, proportion, "remain":"undecided") %>%
  mutate(vote = factor(vote))

brexit_long %>% ggplot(aes(x = enddate, y = proportion, color = vote)) + 
  geom_smooth(method = "loess", span = 0.3) + geom_point()