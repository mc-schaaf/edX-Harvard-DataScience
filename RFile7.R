#####
##### Course 7: Linear Regression
#####
rm(list = ls())
library(tidyverse)
library(Lahman)
?Teams
options(digits = 3)

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

Teams2 %>% summarize(r = cor(R_per_game, AB_per_game))
Teams2 %>% summarize(r = cor(E_per_game, W_per_game))
Teams2 %>% summarize(r = cor(X3B_per_game, X2B_per_game))



set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")

female_heights <- GaltonFamilies%>%    
  filter(gender == "female") %>%    
  group_by(family) %>%    
  sample_n(1) %>%    
  ungroup() %>%    
  select(mother, childHeight) %>%    
  rename(daughter = childHeight)

female_summary <- female_heights %>% summarize(m_M = mean(mother), m_D = mean(daughter), SD_M = sd(mother), SD_D = sd(daughter), M_D_corr = cor(mother, daughter)) %>% as.vector()
female_summary %>% mutate(var_explained = M_D_corr^2)
female_summary %>% mutate(slope_D_givenM = M_D_corr*SD_D/SD_M) %>% mutate(intercept_D_givenM = m_D - (slope_D_givenM*m_M)) %>% mutate(sixty_inch_mother = intercept_D_givenM + 60*slope_D_givenM)






#####
##### 2.2 Least Squares Estimate
#####

rm(list = ls())
library(tidyverse)
library(Lahman)
library(HistData)
data("GaltonFamilies")
set.seed(1983)

galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)
rss <- function(beta0, beta1, data){
  resid <- galton_heights$son - (beta0+beta1*galton_heights$father)
  return(sum(resid^2))
}
beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 36))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss), col=2)


Teams2 <- Teams %>% filter(yearID %in% c(1961:2001)) %>% mutate(
  R_per_game = R/G,
  AB_per_game = AB/G,
  BB_per_game = BB/G,
  HR_per_game = HR/G,
  W_per_game = W/G,
  E_per_game = E/G,
  X3B_per_game = X3B/G,
  X2B_per_game = X2B/G
)
Teams_Model <- lm(R_per_game ~ BB_per_game + HR_per_game, Teams2)
summary(Teams_Model)


rm(list = ls())
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")
options(digits = 3)    # report 3 significant digits

female_heights <- GaltonFamilies %>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight) 

model <- lm(mother ~ daughter, female_heights)
model_intecept <- model$coefficients["(Intercept)"]
model_daughter <- model$coefficients["daughter"]
female_heights[1,] %>% mutate(mother_hat = model_intecept + model_daughter*daughter)
predict(model, female_heights[1,]) ### same but in inplemented function


library(Lahman)
bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)
bat_03 <- Batting %>% filter(yearID %in% c(1999:2001)) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  group_by(playerID) %>%
  summarize(mean_singles = mean(singles), mean_bb = mean(bb)) %>%
  select(playerID, mean_singles, mean_bb)
sum(bat_03$mean_singles > 0.2)
sum(bat_03$mean_bb > 0.2)

bat_04 <- inner_join(bat_02, bat_03)
cor(bat_04$singles, bat_04$mean_singles)
cor(bat_04$bb, bat_04$mean_bb)

plot2 <- ggplot(bat_04)
plot2 + geom_point(aes(x = singles, y = mean_singles))
plot2 + geom_point(aes(x= bb, y = mean_bb))

model <- lm(singles ~ mean_singles, bat_04)
model
model2 <- lm(bb ~ mean_bb, bat_04)
model2



#####
##### 2.3 Tibbles etc.
#####

rm(list = ls())
library(tidyverse)
library(HistData)
data("GaltonFamilies")
set.seed(1) # if you are using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if you are using R 3.6 or later
galton <- GaltonFamilies %>%
  group_by(family, gender) %>%
  sample_n(1) %>%
  ungroup() %>% 
  gather(parent, parentHeight, father:mother) %>%
  mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
  unite(pair, c("parent", "child"))
galton

galton %>%  group_by(pair) %>% count()
galton %>%  group_by(pair) %>% summarize(correlation = cor(parentHeight, childHeight)) %>% arrange(desc(correlation))


tmp1 <- galton %>%  group_by(pair) %>%
  do(tidy(lm(childHeight ~ parentHeight, .), conf.int = TRUE)) %>% filter(term != "(Intercept)")

tmp1 %>% ggplot(aes(pair, y = estimate, ymin = conf.low, ymax = conf.high)) + geom_errorbar() + geom_point() + theme_bw() # + geom_hline(yintercept = 0)
tmp1 %>% ggplot(aes(pair, y = abs(conf.low - conf.high))) + geom_point() + theme_bw() # + geom_hline(yintercept = 0)



#####
##### 2.4 Regression and Baseball
#####

rm(list = ls())
library(tidyverse)
library(Lahman)

Teams %>% filter(yearID == 1971) %>% do(tidy(lm(R ~ BB + HR, .)))

Teams2 <- Teams %>% filter(yearID >= 1961 & yearID <= 2018) %>% group_by(yearID) %>% do(tidy(lm(R ~ BB + HR, .)))
Teams2 %>% filter(term != "(Intercept)") %>% ggplot(aes(y = estimate, x = yearID, color = term)) + geom_point() + geom_hline(yintercept = 0) + geom_smooth(method = "loess")
Teams2 %>% filter(term == "BB") %>% ungroup() %>% do(tidy(lm(estimate ~ yearID, .)))


rm(list = ls())
library(tidyverse)
library(broom)
library(Lahman)
Teams_small <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(
    avg_attendance = attendance/G,
    R_per_game = R/G,
    AB_per_game = AB/G,
    BB_per_game = BB/G,
    HR_per_game = HR/G,
    W_per_game = W/G,
    E_per_game = E/G,
    X3B_per_game = X3B/G,
    X2B_per_game = X2B/G
    )

Teams_small %>% do(tidy(lm(avg_attendance ~ R_per_game, .)))
Teams_small %>% do(tidy(lm(avg_attendance ~ HR_per_game, .)))
Teams_small %>% do(tidy(lm(avg_attendance ~ W, .)))
Teams_small %>% do(tidy(lm(avg_attendance ~ yearID, .)))
Teams_small %>% summarize(cor1 = cor(W, R_per_game), cor2 = cor(W, HR_per_game))

Teams_small %>% mutate(win_strata = round(W/10, digits = 0)) %>% group_by(win_strata) %>% 
  filter(win_strata %in% 5:10) %>% count()
Teams_small %>% mutate(win_strata = round(W/10, digits = 0)) %>% group_by(win_strata) %>% 
  filter(win_strata %in% 5:10) %>%
  do(tidy(lm(avg_attendance ~ R_per_game, .))) %>% filter(term != "(Intercept)") %>% arrange(desc(estimate))
Teams_small %>% mutate(win_strata = round(W/10, digits = 0)) %>% group_by(win_strata) %>% 
  filter(win_strata %in% 5:10) %>%
  do(tidy(lm(avg_attendance ~ HR_per_game, .))) %>% filter(term != "(Intercept)") %>% arrange(desc(estimate))

tmp <- Teams_small %>% do(tidy(lm(avg_attendance ~ R_per_game + HR_per_game + W + yearID, .)))
tmp
tmp2 <- lm(avg_attendance ~ R_per_game + HR_per_game + W + yearID, Teams_small)
tmp2

att2002 <- tmp[1,2] + 5*tmp[2,2] + 1.2*tmp[3,2] + 80*tmp[4,2] + 2002*tmp[5,2]
att1960 <- tmp[1,2] + 5*tmp[2,2] + 1.2*tmp[3,2] + 80*tmp[4,2] + 1960*tmp[5,2]
att2002
att1960

predict(tmp2, data.frame(R_per_game = 5, HR_per_game = 1.2, W = 80, yearID = 2002)) ## only works with direct call of lm, not tidy call

Teams_small2 <- Teams %>% filter(yearID == 2002) %>% as_tibble() %>%
  mutate(
    avg_attendance = attendance/G,
    R_per_game = R/G,
    AB_per_game = AB/G,
    BB_per_game = BB/G,
    HR_per_game = HR/G,
    W_per_game = W/G,
    E_per_game = E/G,
    X3B_per_game = X3B/G,
    X2B_per_game = X2B/G
  )

test <- Teams_small2 %>% mutate(avg_attendance_hat = predict(tmp2, .)) %>% select(teamID, avg_attendance, avg_attendance_hat)
test %>% summarize(cor = cor(avg_attendance, avg_attendance_hat))




#####
##### 3 Correlation and causation
#####
rm(list = ls())
library(tidyverse)
library(dslabs)
library(tidyr)
library(broom)

admissions <- admissions
admissions %>% group_by(gender) %>% summarize(admitted = sum(admitted*applicants*0.01)/sum(applicants)) %>% ungroup() %>% summarize(ratio = admitted[gender == "women"]/admitted[gender == "men"])
admissions %>% ggplot(aes(x = major, y = admitted, color = gender, size = applicants)) + geom_point()


rfr <- research_funding_rates

rfr2 <- rfr %>% summarize(
  noAwards_male = sum(applications_men) - sum(awards_men),
  noAwards_female = sum(applications_women) - sum(awards_women),
  awards_male = sum(awards_men),
  awards_female = sum(awards_women)
  ) %>% gather("key", "total") %>% mutate(
    type = str_split_fixed(key, "_", 2)[,1],
    gender = str_split_fixed(key, "_", 2)[,2]
  ) %>% select(-key) %>% spread(key = type, value = total)


rfr2 %>% mutate(p_awarded = awards/(awards+noAwards)*100)
rfr2[2:3] %>% do(tidy(chisq.test(.)))


dat <- research_funding_rates %>% 
  mutate(discipline = reorder(discipline, success_rates_total)) %>%
  rename(success_total = success_rates_total,
         success_men = success_rates_men,
         success_women = success_rates_women) %>%
  gather(key, value, -discipline) %>%
  separate(key, c("type", "gender")) %>%
  spread(type, value) %>%
  filter(gender != "total")

dat %>% ggplot(aes(x = discipline, y = success, color = gender, size = applications)) + geom_point() + theme_bw()
