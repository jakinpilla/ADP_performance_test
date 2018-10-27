rm(list=ls()); gc()
setwd("C:/Users/Daniel/ADP_performance_test")
getwd()

# 여러 개의 패키지를 한 번에 읽기
Packages <- c('plyr', 'dplyr', 'tidyverse', 'data.table', 'reshape2', 'caret', 'rpart', 'GGally', 
              'ROCR', 'party', 'randomForest', 'e1071')
lapply(Packages, library, character.only=T)

# 데이터 가공 > 예측할 변수 선정 > 데이터 분리 > 모델선택 > 학습 > 평가

# loading data
read.csv('./data/data_total_with_gender_final.csv', stringsAsFactors = F) -> data
head(data)
data$gender <- ifelse(data$gender == "f", 0, 1)
data$gender <- as.factor(data$gender)
data$custid <- as.character(data$custid)
glimpse(data)

data %>%
  select_if(is.numeric) -> var_num

summary(var_num); glimpse(var_num)
# g_paid, buyed_prod_num, pruchased, vdays, day_mean_amt variables are too large :: nomalize 
scale(var_num) -> scaled_var_num # return matrix
as.data.frame(scaled_var_num) -> var_num_df; head(var_num_df)
str(data$gender)

colnames(data)
data %>% 
  select(gender) %>%
  cbind(var_num_df) -> df

glimpse(df)
dim(df)
head(df)

# data splitting----
idx <- createDataPartition(df$gender, p=c(.6, .4), list=F); 
idx[1:10]; length(idx)

# df.train data
df.train <- df[idx, ]
df.valid.test <- df[-idx, ]

dim(df.train)
head(df.train)

# validation data
head(df.valid.test)
idx <- createDataPartition(df.valid.test$gender, p=c(.5, .5), list=F)
idx[1:5]; length(idx)
df.valid <- df.valid.test[idx, ]
df.test <- df.valid.test[-idx, ]

dim(df.valid); head(df.valid)
dim(df.test); head(df.test)

1255 + 417 + 417 # NA 값 등으로 인해 전처리 과정 상 없어진 데이터가 있음에 유의

# model df.train----

## fitControl--
fitControl <- trainControl(method='repeatedcv', number=10, repeats = 3)

## models fitting----
## 로지스틱 회귀--
## 문자형인 변수가 데이터에 들어 있으면 시간 다대 소요됨에 유의할 것
glm_m <- train(gender ~., data=df.train, 
               method = 'glm', 
               family = binomial(link='logit'))

## 의사결정나무(cart)--
cart_m <- train(gender ~., data=df.train, 
                method='rpart',
                trControl = trainControl(method='none', sampling='up'))

## 랜덤포레스트--
rf_m <- train(gender ~., data=df.train, method='rf', 
              trControl=fitControl)
rf_m

## xgBoost--
library(xgboost)
xgboost_m <- train(gender ~., data = df.train, method = 'xgbLinear', 
                   trControl = fitControl)

# 모형 평가 with validation data----
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
# randomForest is winner.

predict(rf_m, df.valid, type='prob') %>% head

# model performance----
df.test_perf <- df.test %>% # model_test
  mutate(GLM = predict(glm_m, df.test),
         CART = predict(cart_m, df.test),
         RF = predict(rf_m, df.test),
         XGB = predict(xgboost_m, df.test))

metrics(df.test_perf, truth=gender, estimate = GLM)
metrics(df.test_perf, truth=gender, estimate = CART)
metrics(df.test_perf, truth=gender, estimate = RF)
metrics(df.test_perf, truth=gender, estimate = XGB)
# randomForest is winner.

varImp(rf_m)

yhat_rf <- predict(rf_m, df.test, type='prob')$`1`
yhat_rf
y_obs <- df.test$gender

# ROC curve and AUC
library(ROCR)
pred_rf <- prediction(yhat_rf, y_obs)
perf_rf <- performance(pred_rf, 'tpr', 'fpr')
plot(perf_rf, main='ROC curve for glm model', col = 'red') 
abline(0,1)
performance(pred_rf, 'auc')@y.values[[1]] # auc


