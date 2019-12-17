library(gtools)
library(tidyverse)
options(digits = 3)    # report 3 significant digits
set.seed(1, sample.kind="Rounding")    # will make R 3.6 generate a seed as in R 3.5

3/(3+5+7)

((5+7)/(2+5+7)) * (3/(3+5+7))

((5+7)/(3+5+7)) * (3/(3+5+7))


#### Discrete probability
nrow(permutations(8, 3)) 
8*7*6

outcomes <- as.data.frame(permutations(8, 3))
mean((outcomes$V1 %in% c(1, 2, 3)) & outcomes$V2 %in% (c(1, 2, 3)) & (outcomes$V3 %in% c(1, 2, 3)))
3/8 *2/7 *1/6

runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
B <- 10000
mean(replicate(B, all(sample(runners, 3) == "Jamaica")))

nrow(combinations(6, 1)) * nrow(combinations(6, 2)) * nrow(combinations(2, 1))
nrow(combinations(6, 1)) * nrow(combinations(6, 2)) * nrow(combinations(3, 1))
nrow(combinations(6, 1)) * nrow(combinations(6, 3)) * nrow(combinations(3, 1))


funfun <- function(ent){
  nrow(combinations(ent, 1)) * nrow(combinations(6, 2)) * nrow(combinations(3, 1))
}


plot(1:12, sapply(1:12, funfun))


funfun2 <- function(sides){
  nrow(combinations(6, 1)) * nrow(combinations(sides, 2)) * nrow(combinations(3, 1))
}

sapply(2:12, funfun2)
plot(2:12, sapply(2:12, funfun2))


rm(list = ls())
library(tidyverse)

esoph <- esoph
nrow(esoph)
all_cases <- sum(esoph$ncases)
all_cases
all_controls <- sum(esoph$ncontrols)
all_controls

esoph %>% filter(alcgp == "120+") %>% summarize(prob = sum(ncases)/ (sum(ncases) + sum(ncontrols)))
esoph %>% filter(alcgp == "0-39g/day") %>% summarize(prob = sum(ncases)/ (sum(ncases) + sum(ncontrols)))
esoph %>% filter(tobgp != "0-9g/day") %>% summarize(prob = sum(ncontrols)/ all_controls)
esoph %>% filter(alcgp == "120+") %>% summarize(prob = sum(ncases)/ all_cases)
esoph %>% filter(tobgp == "30+") %>% summarize(prob = sum(ncases)/ all_cases)
esoph %>% filter(tobgp == "30+" & alcgp == "120+") %>% summarize(prob = sum(ncases)/ all_cases)
esoph %>% filter(tobgp == "30+" | alcgp == "120+") %>% summarize(prob = sum(ncases)/ all_cases)
esoph %>% filter(alcgp == "120+") %>% summarize(prob = sum(ncontrols)/ all_controls)
(esoph %>% filter(alcgp == "120+") %>% summarize(prob = sum(ncases)/ all_cases)) / (esoph %>% filter(alcgp == "120+") %>% summarize(prob = sum(ncontrols)/ all_controls))
esoph %>% filter(tobgp == "30+") %>% summarize(prob = sum(ncontrols)/ all_controls)
esoph %>% filter(tobgp == "30+" & alcgp == "120+") %>% summarize(prob = sum(ncontrols)/ all_controls)
esoph %>% filter(tobgp == "30+" | alcgp == "120+") %>% summarize(prob = sum(ncontrols)/ all_controls)
esoph %>% filter(tobgp == "30+" | alcgp == "120+") %>% summarize(prob = sum(ncases)/ all_cases) / esoph %>% filter(tobgp == "30+" | alcgp == "120+") %>% summarize(prob = sum(ncontrols)/ all_controls)



rm(list = ls())
library(gtools)
library(tidyverse)
options(digits = 3)    # report 3 significant digits
set.seed(16, sample.kind="Rounding")    # will make R 3.6 generate a seed as in R 3.5

mean1 <- 20.9
sd1 <- 5.7
act_scores <- rnorm(10000, mean1, sd1)

mean2 <- mean(act_scores)
sd2 <- sd(act_scores)
sum(act_scores >= 36)
mean(act_scores >= 30)
mean(act_scores <= 10)

x <- 1:36
f_x <- dnorm(x, mean1, sd1)
plot(x, f_x)

act_scores_backup <- act_scores
act_scores <- (act_scores-mean(act_scores))/sd(act_scores)
mean(act_scores > 2)
mean2 + 2*sd2
mean1 + 2*sd1
qnorm(0.975, mean2, sd2)

fun_act <- function(score)
{
  pnorm(score, mean2, sd2)
}

probs <- fun_act(1:36)
which(probs > 0.95)

qnorm(0.95, mean1, sd1)

p <- seq(0.01, 0.99, 0.01)
sample_quantiles <- quantile(act_scores_backup, p)
which(sample_quantiles >= 26)

theoretical_quantiles <- qnorm(p, mean1, sd1)

sample_quantiles <- quantile(act_scores, seq(0.01, 0.99, 0.01)) 
max(which(sample_quantiles <= 26 ))

plot(theoretical_quantiles, sample_quantiles)





rm(list = ls())
library(gtools)
library(tidyverse)
options(digits = 3)    # report 3 significant digits
set.seed(21, sample.kind="Rounding")    # will make R 3.6 generate a seed as in R 3.5

# SE
se1 <- (abs(1 - (-0.25)) * sqrt(0.2*(1-0.2))) * sqrt(44)
se1
1 - pnorm(8, 0, se1)

# MC
B <- 10000
n <- 44
MC_Scores <- replicate(B, {
  sum(sample(c(1, (-0.25)), n, replace = TRUE, prob = c(0.2, 0.8)))
})
mean(MC_Scores >=8)

# New SAT
mean2 <- 44 * 0.25
se2 <- (abs(1 - 0) * sqrt(0.25*(1-0.25))) * sqrt(44)
1 - pnorm(30, mean2, se2)

# skillz
p <- seq(0.25, 0.95, 0.05)
SAT_fun <- function(skill, score){
  tmp_mean <- 44 * skill
  tmp_sd <- (abs(1 - 0) * sqrt(skill*(1-skill))) * sqrt(44)
  return(1 - pnorm(score, tmp_mean, tmp_sd))
}
Probs <- SAT_fun(p, 35)
p[which(Probs > 0.8)]





rm(list = ls())
library(tidyverse)
library(gtools)
options(digits = 3)    # report 3 significant digits
set.seed(21, sample.kind="Rounding")    # will make R 3.6 generate a seed as in R 3.5

E1 <- ((5*6) + ((38-5)*(-1)))/38
E1
SD1 <- abs(6-(-1)) * sqrt((5/38) * (33/38))
SD1
SD1/sqrt(500)
SD1*sqrt(500)
pnorm(0, E1*500, SD1*sqrt(500))







###
### Final Assessment
###
rm(list = ls())
library(tidyverse)
library(gtools)
library(dslabs)
options(digits = 3)    # report 3 significant digits
set.seed(21, sample.kind="Rounding")    # will make R 3.6 generate a seed as in R 3.5

death_prob <- death_prob
head(death_prob)

# females
death <- -150000
alive <- +1150
p <- death_prob %>% filter(age == 50 & sex == "Female")  %>% .$prob
p
E1 <- death*p + alive*(1-p)
E1
SD1 <- abs(death - alive)*sqrt(p*(1-p))
SD1
E1000 <- E1*1000
E1000
SD1000 <- SD1*sqrt(1000)
SD1000
pnorm(0, E1000, SD1000)

#males
p_m <- death_prob %>% filter(age == 50 & sex == "Male") %>% .$prob
p_m
# 
# E[S] = 700 000 = (death*p_m + alive*(1-p_m)) *1 000
# 700 = -150 000*p_m + alive(1-p_m)
# 700 + 150 000*p_m = alive * (1-p_m)
# alive = (700 - 150 000 * p_m) / (1-p_m)
# alive = 

alive_m <- (700 + (150000 * p_m)) / (1-p_m)
alive_m
death_m <- -150000
n <- 1000
Em1 <- (death_m*p_m + alive_m*(1-p_m))*n
Em1
SDm1 <- (abs(death_m-alive_m)*sqrt(p_m*(1-p_m)))*sqrt(n)
SDm1

pnorm(0, Em1, SDm1)

p_pan <- 0.015
E_pan <- (death*p_pan + alive*(1-p_pan))
E_pan*n
SD_pan <- (abs(death - alive))*sqrt(p_pan*(1-p_pan))
SD_pan*sqrt(n)

pnorm(0, E_pan*n, SD_pan*sqrt(n))
pnorm(-1000000, E_pan*n, SD_pan*sqrt(n))

fun_pan <- function(probs){
  E_pan <- (death*probs + alive*(1-probs))
  SD_pan <- (abs(death - alive))*sqrt(probs*(1-probs))
  return(pnorm(0, E_pan*n, SD_pan*sqrt(n)))
}

fun_pan2 <- function(probs){
  E_pan <- (death*probs + alive*(1-probs))
  SD_pan <- (abs(death - alive))*sqrt(probs*(1-probs))
  return(pnorm(-1000000, E_pan*n, SD_pan*sqrt(n)))
}

p <- seq(.01, .03, .001)
p2 <- fun_pan(p)
p[p2>0.9]
p <- seq(.01, .03, .0025)
p3 <- fun_pan2(p)
p[p3>0.9]


set.seed(25, sample.kind = "Rounding")
p_loss <- 0.015
n <- 1000
claim <- -150000
no_claim <- 1150

X <- sum(sample(c(claim, no_claim), n, replace = TRUE, prob = c(p_loss, (1-p_loss))))
X/1000000

rm(X)
set.seed(27, sample.kind = "Rounding")
B <- 10000
X <- replicate(B, {
  sum(sample(c(claim, no_claim), n, replace = TRUE, prob = c(p_loss, (1-p_loss))))
})
mean(X<(-1000000))


rm(list = ls())
p <- 0.015
n <- 1000
l <- -150000
z <-qnorm(0.05)

x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))
x


E1 <- (x*(1-p)) + (l*p)
E1
E1*1000

set.seed(28, sample.kind = "Rounding")
B <- 10000
Y <- replicate(B, {
  mean(sample(c(l, x), n, replace = TRUE, prob = c(p, (1-p))))
})
mean(Y<0)



rm(list = ls())
set.seed(29, sample.kind = "Rounding")
p <- 0.015
n <- 1000
l <- -150000
z <-qnorm(0.05)
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))
B <- 10000

X_2 <- replicate(B,{
  p <- 0.015  + sample(seq(-0.01, 0.01, length = 100), 1)
  return(sum(sample(c(l, x), n, replace = TRUE, prob = c(p, (1-p)))))
})

mean(X_2)
mean(X_2<0)
mean(X_2 < (-1000000))



# 0 > E+z*SE
# -E < z*SE
# 
# -(p_loss*claim + (1-p_loss)*x)*n    <     z* ((x - claim)*sqrt(p_loss*(1-p_loss))*sqrt(n))
# p_loss*claim + (1-p_loss)*x         <     z* ((x - claim)*sqrt(p_loss*(1-p_loss))*sqrt(n)) /(-n)
# p_loss*claim + (1-p_loss)*x         <     (x-claim) *sqrt(p_loss*(1-p_loss)) *z * sqrt(n)/(-n)
# p_loss*claim + (1-p_loss)*x         <     (x-claim) *sqrt(p_loss*(1-p_loss)) *z * n^-0.5
# (p_loss*claim + (1-p_loss)*x) / (x-claim) < sqrt(p_loss*(1-p_loss)) *z * n^-0.5

