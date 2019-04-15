rm(list=ls()); gc()
getwd()

Packages <- c('tidyverse', 'data.table', 'reshape2', 'caret', 'rpart', 'GGally', 
              'ROCR', 'party', 'randomForest', 'e1071')
lapply(Packages, library, character.only=T)

# 데이터 가공 > 예측할 변수 선정 > 데이터 분리 > 모델선택 > 학습 > 평가----

# Date Loading----
data_with_gender <- read_csv('./data/data_total_with_gender_final.csv') 
data_with_gender %>%
  select(custid, gender) -> cust_gender

data_total <- read_csv('./data/data_total.csv') 
data_total %>% colnames()

cust_gender %>%
  left_join(data_total, by = "custid") -> data

data %>% colnames()

data %>%
  select_if(is.numeric) -> var_num
summary(var_num); glimpse(var_num)

# 데이터 정규화 scale()----
scale(var_num) -> scaled_var_num # return matrix
as_tibble(scaled_var_num) -> var_num_tibble; head(var_num_tibble)
str(data$gender)

colnames(data)

data %>% 
  select(gender) %>%
  cbind(var_num_tibble) %>%
  as_tibble() -> data_with_sex.tibble

glimpse(data_with_sex.tibble)

data_with_sex.tibble$gender %>% as.factor()
data_with_sex.tibble$gender  <- factor(data_with_sex.tibble$gender, levels = c("f", "m"), labels = c(0, 1))

df <- data_with_sex.tibble
nrow(df)

library(janitor)
df <- clean_names(df)
df %>% colnames()

# ML----

# Data splitting---
idx <- createDataPartition(df$gender, p=c(.6, .4), list=F); 
idx[1:10]; length(idx)

# df.train data
df.train <- df[idx, ]
df.valid.test <- df[-idx, ]

nrow(df.train)
nrow(df.valid.test)

dim(df.train)
head(df.train)

# validation data-----
head(df.valid.test)
idx <- createDataPartition(df.valid.test$gender, p=c(.5, .5), list=F)
idx[1:5]; length(idx)
df.valid <- df.valid.test[idx, ]
df.test <- df.valid.test[-idx, ]

dim(df.valid); head(df.valid)
dim(df.test); head(df.test)

1255 + 417 + 417 # NA 값 등으로 인해 전처리 과정 상 없어진 데이터가 있음에 유의

# Model df.train

# Setting fitControl----
fitControl <- trainControl(method='repeatedcv', number=10, repeats = 3)

# models fitting----

# glm----
glm_m <- train(gender ~ ., data = df.train, 
               method = 'glm', 
               family = binomial(link='logit'))

glm_m

# cart----
colnames(df.train)
glimpse(df.train)

cart_m <- train(gender ~ ., data = df.train, 
                method = 'rpart',
                trControl = trainControl(method='none', sampling='up'))


# rf----
rf_m <- train(gender ~ ., data = df.train, method='rf', 
              trControl=fitControl)
rf_m

model_arch <- df.valid %>% # model_val
  mutate(GLM  = predict(glm_m, df.valid),
         CART = predict(cart_m, df.valid),
         RF = predict(rf_m, df.valid))

model_pred_result <- model_arch[, c('gender', 'GLM', 'CART', 'RF')] # model_val
head(model_pred_result, 30)

library(yardstick)

metrics(model_arch, truth=gender, estimate = GLM)
metrics(model_arch, truth=gender, estimate = CART)
metrics(model_arch, truth=gender, estimate = RF)

# RF is winner...

varImp(rf_m)

yhat_rf <- predict(rf_m, df.test, type='prob')$`1`
yhat_rf
y_obs <- df.test$gender

# ROC curve and AUC----
library(ROCR)

# prediction pbjection :: probability and labels...
pred_rf <- prediction(yhat_rf, y_obs) 

# performance object :: prediction object, 'tpr', 'fpr'...
perf_rf <- performance(pred_rf, 'tpr', 'fpr')

# ROC curve...
plot(perf_rf, main='ROC curve for glm model', col = 'red') 
abline(0,1)

# auc...
performance(pred_rf, 'auc')@y.values[[1]] # auc

# xgBoost----
library(xgboost)
xgboost_m <- train(gender ~., data = df.train, method = 'xgbLinear', 
                   trControl = fitControl)

# Evaluation with validation data----
model_arch <- df.valid %>% # model_val
  mutate(GLM  = predict(glm_m, df.valid),
         CART = predict(cart_m, df.valid),
         RF = predict(rf_m, df.valid),
         XGB = predict(xgboost_m, df.valid))

model_pred_result <- model_arch[, c('gender', 'GLM', 'CART', 'RF', 'XGB')] # model_val
head(model_pred_result, 30)

library(yardstick)

metrics(model_arch, truth=gender, estimate = GLM)
metrics(model_arch, truth=gender, estimate = CART)
metrics(model_arch, truth=gender, estimate = RF)
metrics(model_arch, truth=gender, estimate = XGB)

predict(rf_m, df.valid, type='prob') %>% head

#' #### model performance
df.test_perf <- df.test %>% # model_test
  mutate(GLM = predict(glm_m, df.test),
         CART = predict(cart_m, df.test),
         RF = predict(rf_m, df.test),
         XGB = predict(xgboost_m, df.test))

metrics(df.test_perf, truth=gender, estimate = GLM)
metrics(df.test_perf, truth=gender, estimate = CART)
metrics(df.test_perf, truth=gender, estimate = RF)
metrics(df.test_perf, truth=gender, estimate = XGB)
