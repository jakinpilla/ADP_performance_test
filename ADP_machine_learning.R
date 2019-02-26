#' ---
#' title: "ADP_machine_learning"
#' author: "jakinpilla"
#' date : "`r format(Sys.time(), '%Y-%m-%d')`"
#' output: 
#'    github_document : 
#'        toc : true
#'        toc_depth : 4
#' ---
#' 
#' 


rm(list=ls()); gc()
getwd()

#' 다음과 같이 여러 패키지들을 한 번에 읽어낼 수 있다.

Packages <- c('plyr', 'dplyr', 'tidyverse', 'data.table', 'reshape2', 'caret', 'rpart', 'GGally', 
              'ROCR', 'party', 'randomForest', 'e1071')
lapply(Packages, library, character.only=T)

#' 데이터 가공 > 예측할 변수 선정 > 데이터 분리 > 모델선택 > 학습 > 평가 등의 과정으로 진행한다.

#' # 가공된 데이터 불러오기
#' 
#' 다음과 같이 가공된 데이터를 불러온다.
data <- read_csv('./data/data_total_with_gender_final.csv') 
head(data)
data$gender <- ifelse(data$gender == "f", 0, 1)
data$gender <- as.factor(data$gender)
data$custid <- as.character(data$custid)
glimpse(data)

data %>%
  rename(hf_6_11 = `hf_6-11`) %>%
  rename(hr_12_17 = `hf_12-17`) %>%
  rename(hr_18_23 = `hf_18-23`) -> data

data %>% colnames()

#' `select_if(is.numeric)` 함수를 이용하면 숫자인 자료형을 가지는 데이터만을 잘 불러올 수 있다.
data %>%
  select_if(is.numeric) -> var_num


summary(var_num); glimpse(var_num)

#' `scale()` 함수를 이용하여 데이터 정규화를 실시한다.
#' 
scale(var_num) -> scaled_var_num # return matrix

#' 위에서 `matrix` 자료형을 반환하였으므로 이를 `tibble`형식으로 변환한다.
#' 
as_tibble(scaled_var_num) -> var_num_tibble; head(var_num_tibble)

#' 예측하려고 하는 변수인 `sex`는 `factor`형 자료형을 가지고 있다.
str(data$gender)

colnames(data)

data %>% 
  select(gender) %>%
  cbind(var_num_tibble) %>%
  as_tibble() -> data_with_sex.tibble

#' `cbind()` 함수를 취하게 되면 자료형이 `data.frame()` 형식으로 전환되므로 이를 tibble 
#' 형식으로 변환하여 변수에 저장하자.

glimpse(data_with_sex.tibble)

#' `ML` 분석을 위해 `data_with_sex.tibble`을 `df`에 할당한다.
df <- data_with_sex.tibble
nrow(df)

#' ### ML 분석
#' 
#'#### Data splitting
#'
idx <- createDataPartition(df$gender, p=c(.6, .4), list=F); 
idx[1:10]; length(idx)

# df.train data
df.train <- df[idx, ]
df.valid.test <- df[-idx, ]

nrow(df.train)
nrow(df.valid.test)

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

#' #### Model df.train

#' ##### Setting fitControl
fitControl <- trainControl(method='repeatedcv', number=10, repeats = 3)
#'
#' ##### models fitting
#' 
#' ######  로지스틱 회귀
#' 문자형인 변수가 데이터에 들어 있으면 시간 다대 소요됨에 유의할 것
glm_m <- train(gender ~ ., data = df.train, 
               method = 'glm', 
               family = binomial(link='logit'))


#' ##### 의사결정나무(cart)
#' 
colnames(df.train)
glimpse(df.train)

#' 만약 ` Error in [.data.frame(m, labs) : undefined columns selected` 같은 경고 메세지가 나온다면
#' 변수 이름을 `janitor` 패키지를 사용하여 청소해주어야 한다.
#' 

library(janitor)

df.train <- clean_names(df.train)
df.train %>% colnames()

#' 
cart_m <- train(gender ~ ., data = df.train, 
                method = 'rpart',
                trControl = trainControl(method='none', sampling='up'))


#' ##### 랜덤포레스트(rf)
rf_m <- train(gender ~ ., data = df.train, method='rf', 
              trControl=fitControl)
rf_m

#' ##### xgBoost
library(xgboost)
xgboost_m <- train(gender ~., data = df.train, method = 'xgbLinear', 
                   trControl = fitControl)

#' #### 모형 평가 with validation data

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

#' 랜덤포레스트의 중요변수를 추출해보자.
varImp(rf_m)

yhat_rf <- predict(rf_m, df.test, type='prob')$`1`
yhat_rf
y_obs <- df.test$gender

#' #### ROC curve and AUC
library(ROCR)

#' `prediction` 개체를 만든다.
#' 
pred_rf <- prediction(yhat_rf, y_obs)

#' 만들어진 `prediction` 개체를 이용, `performance` 개체를 만든다.
perf_rf <- performance(pred_rf, 'tpr', 'fpr')

#' `performance`  개체를 이용해 `ROC curve`를 그린다.
#' 
plot(perf_rf, main='ROC curve for glm model', col = 'red') 
abline(0,1)

#' `AUC`는 `performance` 개체에서 추출할 수 있다. S4 개체에서 `@`를 이용하여 추출한다.
performance(pred_rf, 'auc')@y.values[[1]] # auc



