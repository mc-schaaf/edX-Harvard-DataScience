#####
##### Course 7: Linear Regression
#####

### Course Machine Learning: Titanic Exercise
rm(list = ls())
library(titanic)
library(caret)
library(tidyverse)
library(rpart)
options(digits = 3)

titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)

# Q1
set.seed(42, sample.kind = "Rounding")
index <- createDataPartition(titanic_clean$Survived, times = 1, p = 0.2, list = FALSE)
test_set <- titanic_clean[index,]
train_set <- titanic_clean[-index,]
mean(train_set$Survived == 1)

# Q2
set.seed(3, sample.kind = "Rounding")
test_set$guessing <- sample(c(0, 1), size = length(test_set$Survived), replace = TRUE)
with(test_set, mean(Survived == guessing))

# Q3
with(train_set, table(Survived, Sex))
train_set %>% group_by(Sex) %>% summarize(mean(Survived == "1"))
test_set$feminism <- ifelse(test_set$Sex == "female", 1, 0)
with(test_set, mean(Survived == feminism))

# Q4
train_set %>% group_by(Pclass) %>% summarize(mean(Survived == "1"))
test_set$moneeey <- ifelse(test_set$Pclass == 1, 1, 0)
with(test_set, mean(Survived == moneeey))

train_set %>% group_by(Pclass, Sex) %>% summarize(mean(Survived == "1"))
test_set$more_feminism <- ifelse(test_set$Sex == "female" & test_set$Pclass %in% c(1, 2), 1, 0)
with(test_set, mean(Survived == more_feminism))

test_set <- test_set %>% mutate(
  guessing = as.factor(guessing),
  feminism = as.factor(feminism),
  moneeey = as.factor(moneeey),
  more_feminism = as.factor(more_feminism)
)

# Q5
sapply(c('feminism', 'moneeey', 'more_feminism'), function(x){
  aa <- confusionMatrix(test_set[,x], test_set$Survived)
  aa$byClass[c(1, 2, 11)]
})

# Q6
sapply(c('feminism', 'moneeey', 'more_feminism'), function(x){
  F_meas(test_set[,x], test_set$Survived)
})

# Q7
x <- train_set$Fare %>% as.data.frame()
y <- train_set$Survived
set.seed(1, sample.kind = "Rounding")
fit <- train(x, y, method = "lda")
y_hat <- predict(fit, test_set$Fare %>% as.data.frame())
mean(y_hat == test_set$Survived)

set.seed(1, sample.kind = "Rounding")
fit <- train(x, y, method = "qda")
y_hat <- predict(fit, test_set$Fare %>% as.data.frame())
mean(y_hat == test_set$Survived)

# Q8
x <- train_set$Age %>% as.data.frame()
set.seed(1, sample.kind = "Rounding")
fit <- train(x, y, method = "glm")
y_hat <- predict(fit, test_set$Age %>% as.data.frame())
mean(y_hat == test_set$Survived)

x <- train_set %>% select(Sex, Pclass, Fare, Age)
fit <- train(x, y, method = "glm")
y_hat <- predict(fit, test_set)
mean(y_hat == test_set$Survived)

x <- train_set %>% select(-Survived)
fit <- train(x, y, method = "glm")
y_hat <- predict(fit, test_set)
mean(y_hat == test_set$Survived)

# Q9
set.seed(6, sample.kind = "Rounding")
fit <- train(Survived ~ . , data = train_set, method = "knn", tuneGrid = data.frame(k = seq(3, 51, 2)))
ggplot(fit, highlight = TRUE)
y_hat <- predict(fit, test_set)
mean(y_hat == test_set$Survived)

# Q10
tc <- trainControl(method = "cv", number = 10, p = 0.1)
set.seed(8, sample.kind = "Rounding")
fit <- train(Survived ~ . , data = train_set, method = "knn", trControl = tc, tuneGrid = data.frame(k = seq(3, 51, 2)))
ggplot(fit, highlight = TRUE)
y_hat <- predict(fit, test_set)
mean(y_hat == test_set$Survived)

# Q11
set.seed(10, sample.kind = "Rounding")
fit <- train(Survived ~ . , data = train_set, method = "rpart", tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)))
fit$bestTune
ggplot(fit, highlight = TRUE)
plot(fit$finalModel) + text(fit$finalModel)
y_hat <- predict(fit, test_set)
mean(y_hat == test_set$Survived)

plot(fit$finalModel, margin = 0.1)
text(fit$finalModel)

dat <- test_set[1:7, 2:9]
dat[1,]$Sex <- "male"
dat[1,]$Age <- 28
dat[2,]$Sex <- "female"
dat[2,]$Pclass <- 2
dat[3,]$Sex <- "female"
dat[3,]$Pclass <- 3
dat[3,]$Fare <- 8
dat[4,]$Sex <- "male"
dat[4,]$Age <- 4
dat[5,]$Sex <- "female"
dat[5,]$Pclass <- 3
dat[5,]$Fare <- 25
dat[6,]$Sex <- "female"
dat[6,]$Age <- 17
dat[7,]$Sex <- "male"
dat[7,]$Age <- 17
dat$y_hat <- predict(fit, dat)

# Q12
set.seed(14, sample.kind = "Rounding")
fit <- train(Survived ~ . , data = train_set, method = "rf", tuneGrid = data.frame(mtry = 1:7), ntree = 100)
fit$bestTune
ggplot(fit, highlight = TRUE)
y_hat <- predict(fit, test_set)
mean(y_hat == test_set$Survived)
varImp(fit)













### Course Machine Learning: Breast Cancer Project
rm(list = ls())
options(digits = 3)
library(matrixStats)
library(tidyverse)
library(caret)
library(dslabs)
data(brca)

# Q1
x <- brca$x
y <- brca$y
length(y)
dim(x)
mean(y == "M")
colMeans(x) %>% which.max()
colSds(x) %>% which.min()

# Q2
x <- sweep(x, 2, colMeans(x))
x <- sweep(x, 2, colSds(x), FUN = "/")
colSds(x)[1]
median(x[,1])

# Q3
d <- dist(x) %>% as.matrix() 
ind <- which(y == "M")
mean(d[1, -c(1, ind)])
mean(d[1, ind])

# Q4
d_features <- dist(t(x)) %>% as.matrix()
image(d_features[hclust(dist(d_features))$order,hclust(dist(t(d_features)))$order] 
      #col = RColorBrewer::brewer.pal(11, "Spectral"))
)

heatmap(d_features, labRow = NA, labCol = NA)
cors <- cor(x)
heatmap(cors, labRow = NA, labCol = NA)

# Q5
h <- hclust(dist(t(x)))
groups <- cutree(h, k = 5)
plot(h) 
cbind(colnames(x), groups) %>% as.tibble() %>% group_by(groups) %>% arrange(groups) %>% 
  # filter(V1 %in% c("smoothness_mean", "smoothness_worst"))
  filter(groups == 3) %>% arrange(desc(V1))

# Q6
prco <- prcomp(x)
tmp <- prco$sdev^2/sum(prco$sdev^2)
tmp
which(cumsum(tmp) > 0.9)
summary(prco)

# Q7
ggplot() + geom_point(aes(x = prco$x[,1], y = prco$x[,2], colour = y)) + theme_bw()

# Q8
tmp <- cbind(prco$x, y) %>% as.data.frame()
tmp <- tmp[,c(1:10, 31)]
tmp <- tmp %>% pivot_longer(cols = -y, names_to = "PC", values_to = "values")
tmp %>% ggplot() + geom_boxplot(aes(y = values, color = as.factor(y))) + facet_wrap(~PC) + theme_classic()

# Q9
rm(list = ls())
data(brca)
x_centered <- sweep(brca$x, 2, colMeans(brca$x))
x_scaled <- sweep(x_centered, 2, colSds(brca$x), FUN = "/")
set.seed(1, sample.kind = "Rounding")    # if using R 3.6 or later
test_index <- createDataPartition(brca$y, times = 1, p = 0.2, list = FALSE)
test_x <- x_scaled[test_index,]
test_y <- brca$y[test_index]
train_x <- x_scaled[-test_index,]
train_y <- brca$y[-test_index]
mean(test_y == "B")
mean(train_y == "B")

# Q10
predict_kmeans <- function(x, k) {
  centers <- k$centers    # extract cluster centers
  # calculate distance to cluster centers
  distances <- sapply(1:nrow(x), function(i){
    apply(centers, 1, function(y) dist(rbind(x[i,], y)))
  })
  max.col(-t(distances))  # select cluster with min distance to center
}
set.seed(3, sample.kind = "Rounding")
k <- kmeans(train_x, centers = 2)
kmeans_preds <- ifelse(predict_kmeans(test_x, k) == 1, "B", "M") %>% as.factor()
mean(kmeans_preds == test_y) 
cbind(test_y, kmeans_preds) %>% as_tibble() %>% group_by(test_y) %>% summarize(acc = mean(kmeans_preds == test_y)) 

# Q11
fit_logreg <- train(train_x, train_y, method = "glm")
logreg_preds <- predict(fit_logreg, test_x)
mean(logreg_preds == test_y) 

# Q12
fit_qda <- train(train_x, train_y, method = "qda")
qda_preds <- predict(fit_qda, test_x)
mean(qda_preds == test_y) 

fit_lda <- train(train_x, train_y, method = "lda")
lda_preds <- predict(fit_lda, test_x)
mean(lda_preds == test_y) 

# Q13
set.seed(5, sample.kind = "Rounding")
fit_loess <- train(train_x, train_y, method = "gamLoess")
loess_preds <- predict(fit_loess, test_x)
mean(loess_preds == test_y) 

# Q14
set.seed(7, sample.kind = "Rounding")
fit_knn <- train(train_x, train_y, method = "knn", tuneGrid = data.frame(k = seq(3, 21, 2)))
knn_preds <- predict(fit_knn, test_x)
mean(knn_preds == test_y) 
ggplot(fit_knn, highlight = TRUE)

# Q15
set.seed(9, sample.kind = "Rounding")
fit_rf <- train(train_x, train_y, method = "rf", tuneGrid = data.frame(mtry = seq(3, 9, 2)), importance = TRUE)
rf_preds <- predict(fit_rf, test_x)
mean(rf_preds == test_y) 
ggplot(fit_rf, highlight = TRUE)
varImp(fit_rf)$importance %>% mutate(variable = row.names(.)) %>% arrange(desc(B)) %>% 
  top_n(10, B) %>% mutate(str = str_extract(variable, "_[a-z]+")) %>% 
  group_by(str) %>% count() %>% arrange(desc(n))

# Q16
ensemble <- data.frame(kmeans = kmeans_preds, knn = knn_preds, lda = lda_preds, loess = loess_preds, logreg = logreg_preds, qda = qda_preds, rf = rf_preds)
ensemble$majority <- ifelse(rowMeans(ensemble == "B") < 0.5, "M", "B")
mean(ensemble$majority == test_y)
tmp <- sapply(1:ncol(ensemble), function(x)
{
  mean(ensemble[,x] == test_y)
}) 
colnames(ensemble)[which.max(tmp)]




### 3.2 Comprehension check: smoothing
rm(list = ls())
library(tidyverse)
library(lubridate)
library(purrr)
library(pdftools)

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
dat <- map_df(str_split(pdf_text(fn), "\n"), function(s){
  s <- str_trim(s)
  header_index <- str_which(s, "2015")[1]
  tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
  month <- tmp[1]
  header <- tmp[-1]
  tail_index  <- str_which(s, "Total")
  n <- str_count(s, "\\d+")
  out <- c(1:header_index, which(n==1), which(n>=28), tail_index:length(s))
  s[-out] %>%
    str_remove_all("[^\\d\\s]") %>%
    str_trim() %>%
    str_split_fixed("\\s+", n = 6) %>%
    .[,1:5] %>%
    as_data_frame() %>% 
    setNames(c("day", header)) %>%
    mutate(month = month,
           day = as.numeric(day)) %>%
    gather(year, deaths, -c(day, month)) %>%
    mutate(deaths = as.numeric(deaths))
}) %>%
  mutate(month = recode(month, "JAN" = 1, "FEB" = 2, "MAR" = 3, "APR" = 4, "MAY" = 5, "JUN" = 6, 
                        "JUL" = 7, "AGO" = 8, "SEP" = 9, "OCT" = 10, "NOV" = 11, "DEC" = 12)) %>%
  mutate(date = make_date(year, month, day)) %>%
  filter(date <= "2018-05-01") %>%
  #mutate(date2 = as.numeric(date)) %>%
  filter(is.na(deaths) == FALSE)

span <- 60 / as.numeric(diff(range(dat$date)))
fit <- dat %>% mutate(x = as.numeric(date)) %>% loess(deaths ~ x, data = ., span = span, degree = 1)
dat %>% 
  mutate(smooth = predict(fit), day = yday(date), year = as.character(year(date))) %>%
  ggplot(aes(day, smooth, col = year)) +
  geom_line(lwd = 2)


library(dslabs)
library(broom)
data(mnist_27)

mnist_27$train %>% glm(y ~ x_2, family = "binomial", data = .) %>% tidy()
mnist_27$train %>% loess(as.numeric(y) ~ as.numeric(x_2), data = ., span = 0.2, degree = 1) %>% summary()
mnist_27$train %>% ggplot(aes(x = x_2, y = as.numeric(y))) + geom_point() + geom_smooth(span = 0.2, method.args = list(degree = 1))

fit <- dat %>% mutate(x = as.numeric(date)) %>% loess(deaths ~ x, data = ., span = span, degree = 1)



### 3.3 Comprehension Check: working with matrices
library(tidyverse)
library(dslabs)
if(!exists("mnist")) mnist <- read_mnist()

x <- matrix(rnorm(100*10), 100, 10)
x2 <- x + col(x)
x2 <- sweep(x, 2, 1:ncol(x), FUN = "+")
x2 - x
rowMeans(x)
sapply(x, mean)

class(mnist$train$images)

means <- rowMeans((mnist$train$images > 50 & mnist$train$images < 205))
ggplot() + geom_boxplot(aes(y = means, x = as.factor(mnist$train$labels)))
mean(means)


### 4 Distance, knn, generative models
### 4.1 Comprehension Check: Distance
rm(list = ls())
library(dslabs)
data(tissue_gene_expression)
dim(tissue_gene_expression$x)
table(tissue_gene_expression$y)

d <- dist(tissue_gene_expression$x)
d_m <- as.matrix(d)
d_m[c(1, 39, 73),c(2, 40, 74)]
ind <- c(1, 2, 39, 40, 73, 74)
image(as.matrix(d))
image(as.matrix(d)[ind,ind])

### Comprehension Check: Nearest Neighbour
rm(list = ls())
library(dslabs)
library(tidyverse)
library(caret)

heigths <- heights
set.seed(1, sample.kind = "Rounding")
index <- createDataPartition(heights$sex, times = 1, p = 0.5, list = FALSE)
h_train <- heights[-index,]
h_test <- heights[index,]
k_val <- seq(1, 101, 3)
test1 <- sapply(k_val, function(x)
  {
  fit <- knn3(sex ~ height, data = h_train, k = x)
  y_hat <- predict(fit, h_test, type = "class") %>% factor(levels = levels(h_train$sex))
  F_meas(y_hat, h_test$sex)
  })
max(test1)
k_val[test1 == max(test1)]


rm(list = ls())
set.seed(1, sample.kind = "Rounding")
all_x <- tissue_gene_expression$x
all_y <- tissue_gene_expression$y
index <- createDataPartition(all_y, times = 1, p = 0.5, list = FALSE)
test_x <- all_x[index,]
test_y <- all_y[index]
train_x <- all_x[-index,]
train_y <- all_y[-index]
acc_pred_ks <- function(k_val){
  fit <- knn3(train_x, train_y, k = k_val) 
  y_hat <- predict(fit, test_x, type = "class") %>% factor(levels = levels(all_y))
  mean(y_hat == test_y)
}
ks <- seq(1, 11, by = 2)
accs <- sapply(ks, acc_pred_ks)
plot(ks, accs)
accs

### Comprehension Check: Cross-validation
set.seed(1996, sample.kind="Rounding")
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()
x_subset <- x[ ,sample(p, 100)]
fit <- train(x_subset, y, method = "glm")
fit$results
# install.packages("BiocManager")
# BiocManager::install("genefilter")
library(genefilter)
tt <- colttests(x, y)$p.value
ind <- which(tt < 0.01)
length(ind)
x_subset <- x[ ,ind]
fit <- train(x_subset, y, method = "glm")
fit$results
fit <- train(x_subset, y, method = "knn", tuneGrid = data.frame(k = seq(101, 301, 25)))
ggplot(fit)
all_x <- tissue_gene_expression$x
all_y <- tissue_gene_expression$y
fit <- train(all_x, all_y, method = "knn", tuneGrid = data.frame(k = seq(1,7,2)))
ggplot(fit)


### Comprehension Check: Bootstrap
set.seed(1995, sample.kind="Rounding")
indexes <- createResample(mnist_27$train$y, 10)
sum(indexes[[1]] == 3)
sum(indexes[[1]] == 4)
sum(indexes[[1]] == 7)
sum <- sapply(1:10, function(x)
  {sum(indexes[[x]] == 3)})
sum(sum)
set.seed(1, sample.kind = "Rounding")
B <- 10000 # Mc Distribution
quants <- replicate(B, {
  y = rnorm(100, 0, 1)
  quantile(y, 0.75)
})
mean(quants)
sd(quants)
set.seed(1, sample.kind = "Rounding")
y <- rnorm(100, 0, 1) # Bootstrap Distribution
set.seed(1, sample.kind = "Rounding")
indexes <- createResample(y, times = 10)
quants <- sapply(indexes, function(x)
  {
  y_boot <- y[x]
  quantile(y_boot, 0.75)
})
mean(quants)
sd(quants)
set.seed(1, sample.kind = "Rounding")
y <- rnorm(100, 0, 1) # Bootstrap Distribution
set.seed(1, sample.kind = "Rounding")
indexes <- createResample(y, times = 10000)
quants <- sapply(indexes, function(x)
{
  y_boot <- y[x]
  quantile(y_boot, 0.75)
})
mean(quants)
sd(quants)


### Comprehension Check: Comperative Models
rm(list = ls())
library(dslabs)
library(caret)
library(tidyverse)
data("tissue_gene_expression")

set.seed(1993, sample.kind="Rounding")
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]
fit <- train(x, y, method = "lda")
fit$results
cer <- fit$finalModel$means[1,]
hip <- fit$finalModel$means[2,]
ggplot() + geom_abline(intercept = 0, slope = 1) + 
  geom_text(aes(x = cer, y = hip, label = names(hip))) + theme_classic()


set.seed(1993, sample.kind="Rounding")
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]
fit <- train(x, y, method = "qda")
fit$results
cer <- fit$finalModel$means[1,]
hip <- fit$finalModel$means[2,]
ggplot() + geom_abline(intercept = 0, slope = 1) + 
  geom_text(aes(x = cer, y = hip, label = names(hip))) + theme_classic()


set.seed(1993, sample.kind="Rounding")
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]
fit <- train(x, y, method = "lda", preProcess = "center")
fit$results
cer <- fit$finalModel$means[1,]
hip <- fit$finalModel$means[2,]
ggplot() + geom_abline(intercept = 0, slope = 1) + 
  geom_text(aes(x = 1:length(cer), y = abs(cer - hip), label = names(hip))) + theme_classic()

set.seed(1993, sample.kind="Rounding")
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
x <- x[, sample(ncol(x), 10)]
fit <- train(x, y, method = "lda")
fit$results
confusionMatrix(fit)


### Comprehension Check: Dimension Reduction
rm(list = ls())
library(dslabs)
library(tidyverse)
tge <- tissue_gene_expression
tge$x
pca <- prcomp(tge$x)
qplot(1:length(pca$sdev), pca$sdev)
ggplot() + geom_point(aes(pca$x[,1], pca$x[,2], color = as.factor(ifelse(tge$y == "liver", 1, 0))))
row_m <- rowMeans(tge$x)
ggplot() + geom_point(aes(pca$x[,1], row_m, color = as.factor(tge$y)))
cor.test(pca$x[,1], row_m)
x <- with(tissue_gene_expression, sweep(x, 1, rowMeans(x)))
pc <- prcomp(x)
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) +
  geom_point()
pc_long <- pc$x[,1:10] %>% as.data.frame %>% mutate(rownames = tge$y) %>% pivot_longer(-rownames, names_to = "PC", values_to = "value")
pc_long %>% ggplot() + geom_boxplot(aes(y = value, color = rownames)) + facet_wrap(~PC)
cum <- summary(pc)$importance[3,]
plot(1:length(cum), 1 - cum)






### Comprehension Check: Regularization
rm(list = ls())
library(dslabs)
library(tidyverse)
set.seed(1986, sample.kind="Rounding")
n <- round(2^rnorm(1000, 8, 1))
set.seed(1, sample.kind="Rounding")
mu <- round(80 + 2*rt(1000, 5))
range(mu)
schools <- data.frame(id = paste("PS",1:1000),
                      size = n,
                      quality = mu,
                      rank = rank(-mu))
schools %>% top_n(10, quality) %>% arrange(desc(quality))
set.seed(1, sample.kind="Rounding")
mu <- round(80 + 2*rt(1000, 5))
scores <- sapply(1:nrow(schools), function(i){
  scores <- rnorm(schools$size[i], schools$quality[i], 30)
  scores
})
overall <- mean(sapply(scores, mean))
alpha <- 25
schools <- schools %>% mutate(score = sapply(scores, mean), score_total = sapply(scores, sum))
shrink_fun <- function(a){
  scores_new <- sapply(scores, function(x){
    x - overall
  })
  sapply(scores_new, function(x){
    sum(x)/(length(x) + a) + overall
  })
}
schools %>% top_n(10, score) %>% arrange(desc(score))
schools %>% top_n(10, score) %>% arrange(desc(score)) %>% summarize(median(size))
schools %>% arrange(desc(score)) %>% summarize(median(size))
schools %>% top_n(10, -score) %>% arrange(desc(-score)) %>% summarize(median(size))
schools %>% mutate(top10true = ifelse(id %in% top_n(schools, 10, quality)$id, 1, 0) %>% as.factor()) %>% 
  ggplot(aes(x = size, y = score, size = top10true)) + geom_point() +
  scale_size_discrete(range = c(0,5))
schools <- schools %>% mutate(score_shrink = (score_total - (overall*size))/(size + alpha) + overall)
schools %>% top_n(10, score_shrink) %>% arrange(desc(score_shrink))
schools <- schools %>% mutate(score_shrink = (score_total - (overall*size))/(size + seq_1[which.min(seq_1_RSME)]) + overall)
schools %>% top_n(10, score_shrink) %>% arrange(desc(score_shrink))
seq_1 <- seq(10,250)
seq_1_RSME <- sapply(seq_1, function(alpha){
  shrinks <- shrink_fun(alpha)
  sqrt(mean((schools$quality - shrinks)^2))
})
plot(seq_1, seq_1_RSME)
seq_1[which.min(seq_1_RSME)]
rmse <- sapply(seq_1, function(alpha){
  score_reg <- sapply(scores, function(x) sum(x)/(length(x)+alpha))
  sqrt(mean((score_reg - schools$quality)^2))
})
plot(seq_1, rmse)
seq_1[which.min(rmse)]






### Comprehension Check: Matrix Factorization
rm(list = ls())
library(dslabs)
library(tidyverse)
set.seed(1987, sample.kind="Rounding")
n <- 100
k <- 8
Sigma <- 64  * matrix(c(1, .75, .5, .75, 1, .5, .5, .5, 1), 3, 3) 
m <- MASS::mvrnorm(n, rep(0, 3), Sigma)
m <- m[order(rowMeans(m), decreasing = TRUE),]
y <- m %x% matrix(rep(1, k), nrow = 1) + matrix(rnorm(matrix(n*k*3)), n, k*3)
colnames(y) <- c(paste(rep("Math",k), 1:k, sep="_"),
                 paste(rep("Science",k), 1:k, sep="_"),
                 paste(rep("Arts",k), 1:k, sep="_"))
my_image <- function(x, zlim = range(x), ...){
  colors = rev(RColorBrewer::brewer.pal(9, "RdBu"))
  cols <- 1:ncol(x)
  rows <- 1:nrow(x)
  image(cols, rows, t(x[rev(rows),,drop=FALSE]), xaxt = "n", yaxt = "n",
        xlab="", ylab="",  col = colors, zlim = zlim, ...)
  abline(h=rows + 0.5, v = cols + 0.5)
  axis(side = 1, cols, colnames(x), las = 2)
}
my_image(y)
my_image(cor(y), zlim = c(-1,1))
range(cor(y))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)
s <- svd(y)
names(s)
y_svd <- s$u %*% diag(s$d) %*% t(s$v)
max(abs(y - y_svd))
yv <- y %*% s$v
tmp <- round(t(s$u) %*% s$u, 5) # 24x24, Nahe der Einheitsmatrix
my_image(tmp)
tmp <- round(t(s$v) %*% s$v, 5) # 24x24, Nahe der Einheitsmatrix
my_image(tmp)
my_image(diag(s$d)) # 24x24, Diagonalmatrix
ss_y <- colSums(y^2)
ss_yv <- colSums(yv^2)
sum(ss_y) %>% round() == sum(ss_yv) %>% round()
plot(1:24, ss_y)
plot(1:24, ss_yv)
plot(sqrt(ss_yv), s$d)
sum(ss_yv[1:3])/sum(ss_yv)
identical(s$u %*% diag(s$d), sweep(s$u, 2, s$d, FUN = "*"))
PC1 <- s$u[,1] * s$d[1]
plot(PC1, rowMeans(y))
my_image(s$v)
spread <- sapply(1:24, function(i){
  max(s$v[,i]) - min(s$v[,i])
})
plot(1:24, spread)
y_hat <- s$u[,1] %*% as.matrix(diag(s$d)[1,1]) %*% t(s$v[,1])
my_image(y_hat)
my_image(y)
plot(s$u[,1], ylim = c(-0.25, 0.25))
plot(s$v[,1], ylim = c(-0.25, 0.25))
resid <- y - y_hat
my_image(cor(resid), zlim = c(-1,1))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)
y_hat <- s$u[,2] %*% as.matrix(diag(s$d)[2,2]) %*% t(s$v[,2])
my_image(y_hat)
my_image(resid)
plot(s$u[,2], ylim = c(-0.5, 0.5))
plot(s$v[,2], ylim = c(-0.5, 0.5))
resid <- y - with(s,sweep(u[, 1:2], 2, d[1:2], FUN="*") %*% t(v[, 1:2]))
my_image(cor(resid), zlim = c(-1,1))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)
y_hat <- s$u[,3] %*% as.matrix(diag(s$d)[3,3]) %*% t(s$v[,3])
my_image(y_hat)
my_image(resid)
plot(s$u[,3], ylim = c(-0.5, 0.5))
plot(s$v[,3], ylim = c(-0.5, 0.5))
resid <- y - with(s,sweep(u[, 1:3], 2, d[1:3], FUN="*") %*% t(v[, 1:3]))
my_image(cor(resid), zlim = c(-1,1))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)
y_hat <- s$u[,1:3] %*% as.matrix(diag(s$d)[1:3,1:3]) %*% t(s$v[,1:3])
my_image(y, zlim = c(-30,30))
my_image(y_hat, zlim = c(-30,30))
my_image(resid, zlim = c(-30,30))




### Comprehension Check: Clustering
rm(list = ls())
library(dslabs)
library(tidyverse)
data("tissue_gene_expression")
d <- dist(tissue_gene_expression$x - rowMeans(tissue_gene_expression$x))
h <- hclust(d)
plot(h)

k_means <- kmeans(tissue_gene_expression$x, centers = 7)
ggplot() + geom_point(aes(k_means$cluster, 1:length(k_means$cluster), color = as.factor(ifelse(tissue_gene_expression$y == "liver", 1, 0))))



liver_n_cluster <- function(x)
{
  k_means <- kmeans(tissue_gene_expression$x, centers = 7)
  ind <- which(tissue_gene_expression$y == "liver")
  length(unique(k_means$cluster[ind]))
}

ns <- sapply(1:10^4, liver_n_cluster)
plot(1:10^4, ns[order(ns)])
mean(ns == 1)