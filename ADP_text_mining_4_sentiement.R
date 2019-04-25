#' ---
#' title: "ADP Statistics_1"
#' author: "jakinpilla"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::github_document
#' ---

setwd("/home/insa/ADP_performance_test/")
getwd()

#+ setup, include = FALSE
Packages <- c('plyr', 'dplyr', 'tidyverse', 'data.table', 'reshape2', 'caret', 'rpart', 'GGally', 'ROCR', 
              'randomForest', 'dummies', 'curl', 'gridExtra')
Packages_tm <- c('rJava', 'KoNLP', 'slam', 'wordcloud')

lapply(Packages, library, character.only=T)
lapply(Packages_tm, library, character.only=T); useSejongDic()

#' Data Loading------------------------------------
mobile <- read_csv("data/mobile_2014.csv")
mobile %>%
  write_csv("./data/mobile_2014.csv")

mobile$Texts <- gsub("가-힣", "", mobile$Texts)

glimpse(mobile)
names(mobile)
mobile[2, ]
mobile[1035, ]

#' EDA----
table(mobile$Sentiment)

#' DTM----
library(tm)
corpus <- VCorpus(VectorSource(mobile$Texts))
stopwords()
stopwords("SMART")

dtm <- DocumentTermMatrix(corpus,
                          control = list(tolower = T,
                                         removePunctuation = T,
                                         removeNumbers = T,
                                         stopwords = stopwords("SMART"),
                                         weighting = weightTfIdf))
dtm


#' GLM----
library(glmnet)

X <- as.matrix(dtm)
X[1:5, 1:5]
dim(X)

Y <- mobile$Sentiment
Y[1:5]
Y[1001:1005]

# dtm %>% as.matrix() %>% as.data.table() -> dt_dtm
# mobile$Sentiment %>% 
#   as_tibble() %>%
#   rename(sentiment = value) %>%
#   # mutate(sentiment = as.factor(sentiment)) %>%
#   as.data.table() -> dt_sent
# 
# cbind(dt_dtm, dt_sent) -> dt_dataset
# 
# dt_X <- dt_dataset[, -"sentiment", with = F]
# dt_X %>% class()
# 
# dt_Y <- dt_dataset[, "sentiment"]
# dt_Y %>% class()
# 
# dim(dt_X)
# dim(dt_y)
# dim(dt_dataset)

res.lm <- glmnet(X, Y, 
                 family = "binomial", lambda = 0) # or gaussian

# tuneGrid=expand.grid(
#   .alpha=1,
#   .lambda=seq(0, 100, by = 0.1))
# 
# dt_dataset$sentiment <- as.factor(dt_dataset$sentiment)
# res.lm <-train(sentiment ~ .,
#                data=dt_dataset,
#                method="glmnet",
#                family= "binomial",
#                tuneGrid = expand.grid(lambda = 0, alpha = 0),
#                trControl=trainControl(method = "none"))


res.lm
coef.lm <- coef(res.lm)[,1]
coef.lm[1:5]

coef.lm > 0
pos.lm <- coef.lm[coef.lm > 0]
length(pos.lm)

coef.lm < 0
neg.lm <- coef.lm[coef.lm < 0]
length(neg.lm)

pos.lm <- sort(pos.lm, decreasing = T)
neg.lm <- sort(neg.lm, decreasing = F)

pos.lm[1:5]
neg.lm[1:5]

length(pos.lm)
length(neg.lm)

pos.lm %>% names()

0.1
.1 = 1/10
.01
.001
1e-04

options(scipen = 1)
1e-04
.00001

options(scipen = 100)
.000000000000000000000000000000000000001
options(scipen = 0)
.001
.00000000001
options()$scipen

# overfitting, lambda understanding
# Lasso(diamond), Ridge(circle)
# Lasso(beta_1 = 0, beta_2)
# Ridge(beta_1, beta_2)

res.lasso <- glmnet(X, Y, family = 'binomial', alpha=1)
res.lasso

plot(res.lasso, xvar = 'lambda')

set.seed(12345)
res.lasso <- cv.glmnet(X, Y, family = 'binomial', alpha = 1, # alpha = 1 :: lasso ~!
                       nfolds = 4, type.measure = 'class')
res.lasso

plot(res.lasso)
plot(res.lasso$glmnet.fit, xvar = 'lambda')

coef.lasso <- coef(res.lasso, s = 'lambda.min')[, 1]
coef.lasso

options(scipen = 100)
coef.lasso

pos.lasso <- coef.lasso[coef.lasso > 0]
neg.lasso <- coef.lasso[coef.lasso < 0]

pos.lasso <- sort(pos.lasso, decreasing = T)
neg.lasso <- sort(neg.lasso, decreasing = F)

pos.lasso[1:5]
neg.lasso[1:5]

set.seed(12345)
res.ridge <- cv.glmnet(X, Y, family = 'binomial',
                       alpha = 0, nfolds = 4,  # alpha = 0 :: ridge~!
                       type.measure = 'class')

res.ridge
plot(res.ridge)
plot(res.ridge$glmnet.fit, xvar = 'lambda')

coef.ridge <- coef(res.ridge, s = 'lambda.min')[, 1]
coef.ridge

pos.ridge <- coef.ridge[coef.ridge > 0]
neg.ridge <- coef.ridge[coef.ridge < 0]

pos.ridge <- sort(pos.ridge, decreasing = T)
neg.ridge <- sort(neg.ridge, decreasing = F)

pos.ridge[1:5]
neg.ridge[1:5]

# elastic net...
set.seed(12345)
res.elastic <- cv.glmnet(X, Y, family = 'binomial',
                         alpha = .5, nfolds = 4, # alpha = .5 :: elastic~!
                         type.measure = 'class')
res.elastic
plot(res.elastic)
log(0.02424186)
plot(res.elastic$glmnet.fit, xvar = 'lambda')
coef.elastic <- coef(res.elastic, s = 'lambda.min')[,1]
coef.elastic
pos.elastic <- coef.elastic[coef.elastic > 0]
neg.elastic <- coef.elastic[coef.elastic < 0]
pos.elastic <- sort(pos.elastic, decreasing = T)
neg.elastic <- sort(neg.elastic, decreasing = F)

pos.elastic[1:5]
neg.elastic[1:5]

length(pos.lm)
length(neg.lm)

length(pos.lasso)
length(neg.lasso)

length(pos.ridge)
length(neg.ridge)

length(pos.elastic)
length(neg.elastic)

# lm pos and neg words saving...
pos.lm %>% 
  as_tibble() %>% 
  rownames_to_column() %>%
  rename(pos_words = rowname) %>% 
  write_csv('./data/pos.lm.csv')

pos.lm.csv <- read_csv('./data/pos.lm.csv')
pos.lm.csv[1:5, ]

neg.lm %>%
  as_tibble() %>%
  rownames_to_column() %>%
  rename(neg_words = rowname) %>% 
  write_csv('./data/neg.lm.csv')

neg.lm.csv <- read_csv('./data/neg.lm.csv')
neg.lm.csv[1:5, ]

# lasso pos and neg words saving...
pos.lasso %>% 
  as_tibble() %>% 
  rownames_to_column() %>%
  rename(pos_words = rowname) %>% 
  write_csv('./data/pos.lasso.csv')

pos.lasso.csv <- read_csv('./data/pos.lasso.csv')
pos.lasso.csv[1:5, ]

neg.lasso %>%
  as_tibble() %>%
  rownames_to_column() %>%
  rename(neg_words = rowname) %>%
  write_csv("./data/neg.lasso.csv")

neg.lasso.csv <- read_csv('./data/neg.lasso.csv')
neg.lasso.csv[1:5, ]

# ridge pos and neg words saving...
pos.ridge %>% 
  as_tibble() %>% 
  rownames_to_column() %>%
  rename(pos_words = rowname) %>% 
  write_csv('./data/pos.ridge.csv')

pos.ridge.csv <- read_csv('./data/pos.ridge.csv')
pos.ridge.csv[1:5, ]

neg.ridge %>%
  as_tibble() %>%
  rownames_to_column() %>%
  rename(neg_words = rowname) %>%
  write_csv("./data/neg.ridge.csv")

neg.ridge.csv <- read_csv('./data/neg.ridge.csv')
neg.ridge.csv[1:5, ]

# elastic pos and neg words saving...
pos.elastic %>% 
  as_tibble() %>% 
  rownames_to_column() %>%
  rename(pos_words = rowname) %>% 
  write_csv('./data/pos.elastic.csv')

pos.elastic.csv <- read_csv('./data/pos.elastic.csv')
pos.elastic.csv[1:5, ]

neg.elastic %>%
  as_tibble() %>%
  rownames_to_column() %>%
  rename(neg_words = rowname) %>%
  write_csv("./data/neg.elastic.csv")

neg.elastic.csv <- read_csv('./data/neg.elastic.csv')
neg.elastic.csv[1:5, ]

pos.elastic.csv$pos_words
neg.elastic.csv$neg_words

# scoring test data sentiment with pos and neg words 

mobile.test <- read_csv('./data/mobile2014_test.csv')
dim(mobile.test)
names(mobile.test)

corpus <- VCorpus(VectorSource(mobile.test$Texts))

dtm.test <- DocumentTermMatrix(corpus,
                               control = list(tolower = T,
                                              removePunctuation = T,
                                              removeNumbers = T,
                                              stopwords = stopwords("SMART"),
                                              weighting = weightTfIdf,
                                              dictionary = Terms(dtm)))
dtm.test
dtm

dtm.test %>% as.matrix() -> dtm.test.mat

sum(dtm.test.mat[1, colnames(dtm.test.mat) %in% pos.elastic.csv$pos_words] > 0)
sum(dtm.test.mat[1, colnames(dtm.test.mat) %in% neg.elastic.csv$neg_words] > 0)
mobile.test[1, ]$Texts
mobile.test$Sentiment

nrow(dtm.test.mat)

# Senti_score.lm var---
lm.senti_vec <- c()
for (i in 1:nrow(dtm.test.mat)) {
  ith_pos.n <- sum(dtm.test.mat[i, colnames(dtm.test.mat) %in% pos.lm.csv$pos_words] > 0)
  ith_neg.n <- sum(dtm.test.mat[i, colnames(dtm.test.mat) %in% neg.lm.csv$neg_words] > 0)
  ith_senti_score <- ith_pos.n - ith_neg.n
  lm.senti_vec <- c(lm.senti_vec, ith_senti_score)
}

lm.senti_vec

mobile.test %>%
  mutate(Senti_score.lm = lm.senti_vec) %>%
  mutate(Sentiment.lm = ifelse(Senti_score.lm <= 0, 0, 1)) -> sent.test.result_1

sent.test.result_1

# Senti_score.lasso var---
lasso.senti_vec <- c()
for (i in 1:nrow(dtm.test.mat)) {
  ith_pos.n <- sum(dtm.test.mat[i, colnames(dtm.test.mat) %in% pos.lasso.csv$pos_words] > 0)
  ith_neg.n <- sum(dtm.test.mat[i, colnames(dtm.test.mat) %in% neg.lasso.csv$neg_words] > 0)
  ith_senti_score <- ith_pos.n - ith_neg.n
  lasso.senti_vec <- c(lasso.senti_vec, ith_senti_score)
}

lasso.senti_vec

sent.test.result_1 %>%
  mutate(Senti_score.lasso = lasso.senti_vec) %>%
  mutate(Sentiment.lasso = ifelse(Senti_score.lasso <= 0, 0, 1)) -> sent.test.result_2

sent.test.result_2 %>% View()

# Senti_score.ridge var---
ridge.senti_vec <- c()
for (i in 1:nrow(dtm.test.mat)) {
  ith_pos.n <- sum(dtm.test.mat[i, colnames(dtm.test.mat) %in% pos.ridge.csv$pos_words] > 0)
  ith_neg.n <- sum(dtm.test.mat[i, colnames(dtm.test.mat) %in% neg.ridge.csv$neg_words] > 0)
  ith_senti_score <- ith_pos.n - ith_neg.n
  ridge.senti_vec <- c(ridge.senti_vec, ith_senti_score)
}

ridge.senti_vec

sent.test.result_2 %>%
  mutate(Senti_score.ridge = ridge.senti_vec) %>%
  mutate(Sentiment.ridge = ifelse(Senti_score.ridge <= 0, 0, 1)) -> sent.test.result_3

sent.test.result_3 %>% View()

# Senti_score.elastic var---
elastic.senti_vec <- c()
for (i in 1:nrow(dtm.test.mat)) {
  ith_pos.n <- sum(dtm.test.mat[i, colnames(dtm.test.mat) %in% pos.elastic.csv$pos_words] > 0)
  ith_neg.n <- sum(dtm.test.mat[i, colnames(dtm.test.mat) %in% neg.elastic.csv$neg_words] > 0)
  ith_senti_score <- ith_pos.n - ith_neg.n
  elastic.senti_vec <- c(elastic.senti_vec, ith_senti_score)
}

elastic.senti_vec

sent.test.result_3 %>%
  mutate(Senti_score.elastic = elastic.senti_vec) %>%
  mutate(Sentiment.elastic = ifelse(Senti_score.elastic <= 0, 0, 1)) -> sent.test.result_4

sent.test.result_4
sent.test.result_4 -> mobile.test.result

mobile.test.result %>%
  select(contains("Sentiment")) %>% View()

library(caret)
mobile.test.result %>%
  mutate(Sentiment = as.factor(Sentiment)) %>%
  mutate(Sentiment.lm = as.factor(Sentiment.lm)) %>%
  mutate(Sentiment.lasso = as.factor(Sentiment.lasso)) %>%
  mutate(Sentiment.ridge = as.factor(Sentiment.ridge)) %>%
  mutate(Sentiment.elastic = as.factor(Sentiment.elastic)) -> mobile.test.result

# evaluate lm model...
confusionMatrix(mobile.test.result$Sentiment, mobile.test.result$Sentiment.lm)

# evaluate lasso model...

# to fix different levels...
mobile.test.result %>%
  select(Sentiment.lasso) %>% unique()
str(mobile.test.result)
mobile.test.result$Sentiment.lasso <- factor(mobile.test.result$Sentiment.lasso, levels = c(0, 1))

confusionMatrix(mobile.test.result$Sentiment, mobile.test.result$Sentiment.lasso)

# evaluate ridge model...
confusionMatrix(mobile.test.result$Sentiment, mobile.test.result$Sentiment.ridge)

# evaluate elastic model...
confusionMatrix(mobile.test.result$Sentiment, mobile.test.result$Sentiment.elastic)

# accs per each models...
lm.acc <- confusionMatrix(mobile.test.result$Sentiment, mobile.test.result$Sentiment.lm)$overall[[1]]
lasso.acc <- confusionMatrix(mobile.test.result$Sentiment, mobile.test.result$Sentiment.lasso)$overall[[1]]
ridge.acc <- confusionMatrix(mobile.test.result$Sentiment, mobile.test.result$Sentiment.ridge)$overall[[1]]
elastic.acc <- confusionMatrix(mobile.test.result$Sentiment, mobile.test.result$Sentiment.elastic)$overall[[1]]

acc <- c(lm.acc, lasso.acc, ridge.acc, elastic.acc)
names(acc) <- c('lm', 'lasso', 'ridge', 'elastic')
acc %>%
  enframe()
# ridge is winner...

library(yardstick)
metrics(mobile.test.result, truth=Sentiment, estimate = Sentiment.lm)
metrics(mobile.test.result, truth=Sentiment, estimate = Sentiment.lasso)
metrics(mobile.test.result, truth=Sentiment, estimate = Sentiment.ridge)
metrics(mobile.test.result, truth=Sentiment, estimate = Sentiment.elastic)

# vsualization...
mobile.test.result %>%
  ggplot(aes(Sentiment.ridge, fill = Sentiment.ridge)) + geom_histogram(stat = "count")

mobile.test.result %>%
  filter(Senti_score.ridge >= -25 & Senti_score.ridge <= 25) %>%
  ggplot(aes(Senti_score.ridge, fill = Sentiment.ridge)) + geom_histogram(stat = "count")


# with tablet2014 dataset----
data.test <- read_csv('./data/tablet2014_test.csv')
data.test %>%
  filter(grepl("stuck", Texts)) %>% 
  select(Texts) %>%
  slice(11) %>% as.data.frame() -> df.sample

gsub("<U+0095>", "", df.sample$Texts) -> df.sample.text
gsub("u0095", "", df.sample.text)

corpus <- VCorpus(VectorSource(data.test$Texts))

dtm.test <- DocumentTermMatrix(corpus,
                               control = list(tolower = T,
                                              removePunctuation = T,
                                              removeNumbers = T,
                                              stopwords = stopwords("SMART"),
                                              weighting = weightTfIdf,
                                              dictionary = Terms(dtm)))

library(tm.plugin.sentiment)

senti.lm.test <- polarity(dtm.test, names(pos.lm), names(neg.lm))
senti.lasso.test <- polarity(dtm.test, names(pos.lasso), names(neg.lasso))
senti.ridge.test <- polarity(dtm.test, names(pos.ridge), names(neg.ridge))
senti.elastic.test <- polarity(dtm.test, names(pos.elastic), names(neg.elastic))

senti.lm.b.test <- ifelse(senti.lm.test > 0, 1, 0)
senti.lasso.b.test <- ifelse(senti.lasso.test > 0, 1, 0)
senti.ridge.b.test <- ifelse(senti.ridge.test > 0, 1, 0)
senti.elastic.b.test <- ifelse(senti.elastic.test > 0, 1, 0)

library(caret)

confusionMatrix(senti.lm.b.test, data.test$Sentiment)
confusionMatrix(senti.lasso.b.test, data.test$Sentiment)
confusionMatrix(senti.ridge.b.test, data.test$Sentiment)
confusionMatrix(senti.elastic.b.test, data.test$Sentiment)

lm.acc <- confusionMatrix(senti.lm.b.test, data.test$Sentiment)$overall[1]
lasso.acc <- confusionMatrix(senti.lasso.b.test, data.test$Sentiment)$overall[1]
ridge.acc <- confusionMatrix(senti.ridge.b.test, data.test$Sentiment)$overall[1]
elastic.acc <- confusionMatrix(senti.elastic.b.test, data.test$Sentiment)$overall[1]
acc <- c(lm.acc, lasso.acc, ridge.acc, elastic.acc)
names(acc) <- c('lm', 'lasso', 'ridge', 'elastic')

tablet.acc <- acc
tablet.acc

## test(books)

data.test <- read.csv('books_test.csv',  stringsAsFactors = F)

corpus <- VCorpus(VectorSource(data.test$Texts))

dtm.test <- DocumentTermMatrix(corpus,
                               control = list(tolower = T,
                                              removePunctuation = T,
                                              removeNumbers = T,
                                              stopwords = stopwords("SMART"),
                                              weighting = weightTfIdf,
                                              dictionary = Terms(dtm)))

library(tm.plugin.sentiment)

senti.lm.test <- polarity(dtm.test, names(pos.lm), names(neg.lm))
senti.lasso.test <- polarity(dtm.test, names(pos.lasso), names(neg.lasso))
senti.ridge.test <- polarity(dtm.test, names(pos.ridge), names(neg.ridge))
senti.elastic.test <- polarity(dtm.test, names(pos.elastic), names(neg.elastic))

senti.lm.b.test <- ifelse(senti.lm.test > 0, 1, 0)
senti.lasso.b.test <- ifelse(senti.lasso.test > 0, 1, 0)
senti.ridge.b.test <- ifelse(senti.ridge.test > 0, 1, 0)
senti.elastic.b.test <- ifelse(senti.elastic.test > 0, 1, 0)

library(caret)

confusionMatrix(senti.lm.b.test, data.test$Sentiment)
confusionMatrix(senti.lasso.b.test, data.test$Sentiment)
confusionMatrix(senti.ridge.b.test, data.test$Sentiment)
confusionMatrix(senti.elastic.b.test, data.test$Sentiment)

lm.acc <- confusionMatrix(senti.lm.b.test, data.test$Sentiment)$overall[1]
lasso.acc <- confusionMatrix(senti.lasso.b.test, data.test$Sentiment)$overall[1]
ridge.acc <- confusionMatrix(senti.ridge.b.test, data.test$Sentiment)$overall[1]
elastic.acc <- confusionMatrix(senti.elastic.b.test, data.test$Sentiment)$overall[1]
acc <- c(lm.acc, lasso.acc, ridge.acc, elastic.acc)
names(acc) <- c('lm', 'lasso', 'ridge', 'elastic')

books.acc <- acc
books.acc

mobile.acc
tablet.acc
books.acc

## ??��?????? ȸ?Ͱ????? ?̿??? ??��?м?

data.test <- read.csv('tablet2014_test.csv', stringsAsFactors = F)

corpus <- VCorpus(VectorSource(data.test$Texts))
dtm.test <- DocumentTermMatrix(corpus,
                               control = list(tolower = T,
                                              removePunctuation = T,
                                              removeNumbers = T,
                                              stopwords = stopwords("SMART"),
                                              weighting = weightTfIdf,
                                              dictionary = Terms(dtm)))

X.test <- as.matrix(dtm.test)
senti.lm.test.coef <- predict(res.lm , newx = X.test)
senti.lasso.test.coef <- predict(res.lasso, newx = X.test, s = "lambda.min")
senti.ridge.test.coef <- predict(res.ridge, newx = X.test, s = "lambda.min")
senti.elastic.test.coef <- predict(res.elastic, newx = X.test, s = "lambda.min")

senti.lm.test.coef
senti.lasso.test.coef
senti.ridge.test.coef
senti.elastic.test.coef

senti.lm.b.test.coef <- ifelse(senti.lm.test.coef > 0, 1, 0)
senti.lasso.b.test.coef <- ifelse(senti.lasso.test.coef > 0, 1, 0)
senti.ridge.b.test.coef <- ifelse(senti.ridge.test.coef > 0, 1, 0)
senti.elastic.b.test.coef <- ifelse(senti.elastic.test.coef > 0, 1, 0)

confusionMatrix(senti.lm.b.test.coef, data.test$Sentiment)$overall[1]
confusionMatrix(senti.lasso.b.test.coef, data.test$Sentiment)$overall[1]
confusionMatrix(senti.ridge.b.test.coef, data.test$Sentiment)$overall[1]
confusionMatrix(senti.elastic.b.test.coef, data.test$Sentiment)$overall[1]

# visualization









