library(tidyverse)
library(dslabs)


### Section 1
url <- "http://mlr.cs.umass.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"
dat <- read_csv(url, col_names = FALSE)
nrow(dat)
ncol(dat)
url <- "ftp://aftp.cmdl.noaa.gov/products/trends/co2/co2_annmean_mlo.txt"
dat <- read_table(url, skip = 56)
nrow(dat)
ncol(dat)


### Section 2.1
rm(list = ls())
key <- c("allen_height", "allen_hand_length", "allen_wingspan", "bamba_height", "bamba_hand_length", "bamba_wingspan")
value <- c(75, 8.25, 79.25, 83.25, 9.75, 94)
df1 <- data.frame(key, value)

tidy_data <- df1 %>%
  separate(col = key, into = c("player", "variable_name"), sep = "_", extra = "merge") %>% 
  spread(key = variable_name, value = value)

tidy_data2 <- df1 %>%
  separate(col = key, into = c("player", "variable_name1", "variable_name2"), sep = "_", fill = "right") %>% 
  unite(col = variable_name, variable_name1, variable_name2, sep = "_") %>% 
  spread(key = variable_name, value = value)

tidy_data3 <- df1 %>%
  separate(col = key, into = c("player", "variable_name"), sep = "_") %>% 
  spread(key = variable_name, value = value)


co2
o2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) %>% 
  setNames(1:12) %>%
  mutate(year = as.character(1959:1997))
co2_tidy <- gather(o2_wide,month,co2,-year)
co2_tidy %>% ggplot(aes(as.numeric(month), co2, color = year)) + geom_line()

rm(list = ls())
data(admissions)
dat <- admissions %>% select(-applicants)
dat_tidy <- spread(dat, gender, admitted)

tmp <- gather(admissions, key, value, admitted:applicants)
tmp
tmp2 <- unite(tmp, column_name, c(key, gender))
tmp2
tmp3 <- spread(tmp2, column_name, value)
tmp3



### Section 2.2
df1 <- data.frame(x = c("a", "b"), y = c("a", "a"))
df2 <- data.frame(x = c("a", "a"), y = c("a", "b"))
final <- setdiff(df1, df2)

library(Lahman)
top <- Batting %>% 
  filter(yearID == 2016) %>%
  arrange(desc(HR)) %>%    # arrange by descending HR count
  slice(1:10)    # take entries 1-10
top %>% as_tibble()
Master %>% as_tibble()

top_names <- top %>% left_join(Master) %>%
  select(playerID, nameFirst, nameLast, HR)

top_salary <- Salaries %>% filter(yearID == 2016) %>%
  right_join(top_names) %>%
  select(nameFirst, nameLast, teamID, HR, salary)

AwardsPlayers %>% filter(yearID == 2016) %>% .$playerID %>% intersect(top_names$playerID) %>% length()
AwardsPlayers %>% filter(yearID == 2016) %>% .$playerID %>% setdiff(top_names$playerID) %>% length()

c <- 1:4 %>% as.data.frame()
d <- 5:6 %>% as.data.frame()

left_join(c, d)


### Section 2.3
library(rvest)
library(tidyverse)
h <- read_html("http://www.foodnetwork.com/recipes/alton-brown/guacamole-recipe-1940609")
recipe <- h %>% html_node(".o-AssetTitle__a-HeadlineText") %>% html_text()
prep_time <- h %>% html_node(".m-RecipeInfo__a-Description--Total") %>% html_text()
ingredients <- h %>% html_nodes(".o-Ingredients__a-Ingredient") %>% html_text()
guacamole <- list(recipe, prep_time, ingredients)
guacamole



url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)
nodes <- html_nodes(h, "table")
html_text(nodes[[8]])
dat1 <- html_table(nodes[[1]])
dat2 <- html_table(nodes[[2]])
dat3 <- html_table(nodes[[3]])
dat4 <- html_table(nodes[[4]])
sapply(nodes[19:21], html_table)


tab_1 <- html_table(nodes[[10]])
tab_2 <- html_table(nodes[[19]])
tab_1 <- tab_1[-1, -1] #%>% names(c("Team", "Payroll", "Average"))
tab_2 <- tab_2[-1,] # %>% names(c("Team", "Payroll", "Average"))
names(tab_1) <- c("Team", "Payroll", "Average")
names(tab_2) <- c("Team", "Payroll", "Average")
full_join(tab_1, tab_2, by = "Team")

rm(list = ls())
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
h <- read_html(url)
nodes <- html_nodes(h, "table")
tmp <- c(NA, NA)
tmp2 <- c(NA, NA)
for (i in 1:length(nodes)) {
  tmp[i] <- html_table(nodes[i], fill = TRUE) %>% as.data.frame() %>% ncol()
  # tmp2[i] <- html_table(nodes[i], fill = TRUE) %>% as.data.frame() %>% names()
}
for (i in which(tmp == 9)){
  X <- html_table(nodes[i], fill = TRUE) %>% as.data.frame()
   assign(paste0(as.character("dat"), as.character(i)), X)
}




#### Section 3.1
library(tidyverse)
rm(list = ls())

asdf <- c("123,456,789", "werweis38dennsowas19")
asdf

test_1 <- str_replace_all(asdf, ",", "")
test_1 <- as.numeric(test_1)

test_1 <- parse_number(asdf)
test_1 <- as.numeric(test_1)






#### Section 3.3
rm(list = ls())
library(rvest)
library(tidyverse)
library(stringr)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
tab <- read_html(url) %>% html_nodes("table")
polls <- tab[[5]] %>% html_table(fill = TRUE)

polls <- polls %>% `colnames<-`(c("dates", "remain", "leave", "undecided", "lead", "samplesize", "pollster", "poll_type", "notes"))
polls <- polls %>% filter(str_detect(.$remain, ".%"))
nrow(polls)
polls2 <- polls 
polls2$undecided <- polls2$undecided %>% str_replace("N/A", "0")


temp <- str_extract_all(polls$dates, "\\d+\\s[a-zA-Z]+")
end_date <- sapply(temp, function(x) x[length(x)]) # take last element (handles polls that cross month boundaries)



#### Section 3.4
rm(list = ls())
library(rvest)
library(tidyverse)
library(stringr)
library(dslabs)
library(lubridate)
library(gutenbergr)
library(tidytext)
options(digits = 3)

data(brexit_polls)

brexit_polls %>% filter(month(startdate) == 4) %>% count()
brexit_polls %>% mutate(end_week = round_date(enddate, unit = "week"))  %>% filter(end_week ==  "2016-06-12") %>% count(end_week)
brexit_polls %>% mutate(week_day = weekdays(enddate)) %>% count(week_day) %>% arrange(desc(n))


data(movielens)
movielens %>% mutate(timestamp2 = as_datetime(timestamp)) %>% count(year(timestamp2)) %>% arrange(desc(n))
movielens %>% mutate(timestamp2 = as_datetime(timestamp)) %>% count(hour(timestamp2)) %>% arrange(desc(n))

gutenberg_works(str_detect(title, "[pP]ride\\s*[a-zA-Z]*\\s*[pP]rejudice")) %>% filter(!is.na(author))
PP_book <- gutenberg_works(str_detect(title, "[pP]ride\\s*[a-zA-Z]*\\s*[pP]rejudice")) %>% filter(!is.na(author)) %>% gutenberg_download()
PP_words <- PP_book %>% unnest_tokens(word, text)
PP_words %>% count()
PP_words %>% filter(!(word %in% stop_words$word)) %>% count()
PP_words %>% filter(!(word %in% stop_words$word)) %>% filter(!str_detect(word, "\\d")) %>% count()
PP_words %>% filter(!(word %in% stop_words$word)) %>% filter(!str_detect(word, "\\d")) %>% count(word) %>% arrange(desc(n))
PP_words %>% filter(!(word %in% stop_words$word)) %>% filter(!str_detect(word, "\\d")) %>% count(word) %>% filter(n > 100) %>% count()
PP_words2 <- PP_words %>% filter(!(word %in% stop_words$word)) %>% filter(!str_detect(word, "\\d"))

afinn <- get_sentiments("afinn")
afinn_sentiments <- inner_join(PP_words2, afinn, by = "word")
afinn_sentiments %>% count()
mean((afinn_sentiments$value > 0))
mean(afinn_sentiments$value > 0)
afinn_sentiments %>% filter(value == 4) %>% count()






#### Section 4: Wrap up
rm(list = ls())
library(tidyverse)
library(pdftools)
options(digits = 3)    # report 3 significant digits

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
system("cmd.exe", input = paste("start", fn))


txt <- pdf_text(fn)
txt

x <- str_split(txt[9], "\n")
x

s <- x[[1]]
s

s <- str_trim(s)
s[1]

header_index <- min(str_which(s, "2015"))
header_index

header <- s[header_index]
header
header <- str_split(header, "\\s+", simplify = TRUE)
month <- header[,1]
header <- header[,-1]
header[3]

tail_index <- min(str_which(s, "[tT]otal"))
tail_index

n <- str_count(s, "(\\s*\\d+\\s*)")
sum(n == 1)


s2 <- s[(header_index+1):(tail_index-1)]
s3 <- s2[str_count(s2, "(\\s*\\d+\\s*)") != 1]
s <- s3
# ### more elegant
# out <- c(1:header_index, which(n==1), tail_index:length(s))
# s <- s[-out]

s <- str_remove_all(s, "[^\\d\\s]")
s2 <- str_split_fixed(s, "\\s+", n = 6)[,1:5]

s2 <- as.data.frame(s2)
names(s2) <- c("day", header)
s2$month <- month
s3 <- s2 %>% mutate_all(as.character) %>% mutate_at(1:5, as.numeric)
### more elegant


mean(s3$'2015')
mean(s3$'2016')
mean(s3$'2017'[s3$day <=19])
mean(s3$'2017'[s3$day > 19])



tab <- s3 %>% gather(year, deaths, -c(day, month)) %>%
  mutate(deaths = as.numeric(deaths))
tab

plot1 <- tab %>% filter(year != 2018) %>% ggplot(aes(x = day, y = deaths))
plot1 + geom_line(aes(color = year))
