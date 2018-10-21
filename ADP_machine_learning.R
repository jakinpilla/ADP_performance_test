setwd("C:/Users/Daniel/ADP_performance_test")
getwd()

# install.packages('yardstick')
# install.packages('party')
# install.packages('randomForest')

# 여러 개의 패키지를 한 번에 읽기
Packages <- c('plyr', 'dplyr', 'tidyverse', 'data.table', 'reshape2', 'caret', 'rpart', 'GGally', 'ROCR', 'party', 
              'randomForest', 'e1071')
lapply(Packages, library, character.only=T)

# 데이터 가공 > 예측할 변수 선정 > 데이터 분리 > 모델선택 > 학습 > 평가

# loading data
read.csv('./data/data_total.csv') -> data
glimpse(data)

data %>%
  select_if(is.numeric) -> var_num

summary(var_num)
# g_paid, buyed_prod_num, pruchased, vdays, day_mean_amt variables are too large :: nomalize 
scale(var_num) -> scaled_var_num
as.data.frame(scaled_var_num) -> var_num_df; head(var_num_df)

str(data$gender)
data$gender <- ifelse(data$gender == "f", 0, 1)
data$custid <- as.character(data$custid)

data %>% 
  select(custid, gender) %>%
  cbind(var_num_df) -> df

glimpse(df)
as.factor(df$gender) -> df$gender; glimpse(df)
dim(df)

# data splitting----
idx <- createDataPartition(df$gender, p=c(.6, .4), list=F); 
idx[1:10]; length(idx)

# df.train data
df.train <- df[idx, ]
df.valid.test <- df[-idx, ]

dim(df.train)

# validation data
idx <- createDataPartition(df.valid.test$gender, p=c(.5, .5), list=F)
idx[1:5]; length(idx)
df.valid <- df.valid.test[idx, ]
df.test <- df.valid.test[-idx, ]

dim(df.valid)
dim(df.test)

1255 + 417 + 417 # NA 값 등으로 인해 전처리 과정 상 없어진 데이터가 있음에 유의

# model df.train----

## fitControl--
fitControl <- trainControl(method='repeatedcv', number=10)

## models fitting----

## 로지스틱 회귀--
## 시간 소요 다대...극복방안은?
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


## xgBoost--
library(xgboost)
xgboost_m <- train(gender ~., data = df.train, method = 'xgbLinear', 
                   trControl = fitControl)

# 모형 평가 with validation data----
model_arch <- validation %>% # model_val
  mutate(GLM  = predict(glm_m, validation),
         CART = predict(cart_m, validation),
         RF = predict(rf_m, validation),
         XGB = predict(xgboost_m, validation),
         GBM = predict(gbm_m, validation))

model_arch[, c('Class', 'CART', 'RF', 'XGB', 'GBM')] # model_val




