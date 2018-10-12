setwd("C:/Users/Daniel/ADP_performance_test")
getwd()

# install.packages('yardstick')
# install.packages('party')
# install.packages('randomForest')

# 여러 개의 패키지를 한 번에 읽기
Packages <- c('tidyverse', 'data.table', 'reshape2', 'caret', 'rpart', 'GGally', 'ROCR', 'party', 
              'randomForest', 'dplyr')
lapply(Packages, library, character.only=T)

# 데이터 가공 > 예측할 변수 선정 > 데이터 분리 > 모델선택 > 학습 > 평가

# titanic data 가공하기
## data loading
as.tibble(fread('./data/titanic3.csv', data.table = F)) -> titanic
glimpse(titanic)

# var "sex" preprocessing as 0(female), 1(male)
as.factor(titanic$sex) -> titanic$sex
(as.numeric(titanic$sex) - 1) -> titanic$sex #female -> 0, male -> 1
glimpse(titanic)

# var "suvivied " preprocessing as factor with labels c("dead", "survived")
titanic$survived <- factor(titanic$survived, levels=c(0,1), labels=c('dead', 'survived'))
glimpse(titanic)

# var "embarked" preprocessing
unique(titanic$embarked)
titanic %>% 
  filter(embarked != "") -> titanic
titanic$embarked <- as.factor(titanic$embarked)
titanic$embarked <- as.numeric(titanic$embarked) # "S" : 3, "C" : 1, "Q" : 2
glimpse(titanic)

titanic %>% 
  select_if(is.numeric) %>% 
  select(-body) %>%
  replace(is.na(.), 0) %>%
  cbind(survived = titanic$survived) -> titanic_preprocessed
glimpse(titanic_preprocessed)

# 예측할 변수를 'survived'로 선택
levels(titanic$survived)
prop.table(table(titanic_preprocessed$survived))

# 1) 학습/평가 데이터셋 분리
nrow(titanic_preprocessed)
idx <- createDataPartition(titanic_preprocessed$survived, p=.8)[[1]] # createDataPartition() 기본 리스트 반환

titanic.train <- titanic_preprocessed[idx, ]
titanic.test <- titanic_preprocessed[-idx, ]
head(titanic.train)
head(titanic.test)

# survived and dead ratio check between train dataset and test dataset
prop.table(table(titanic.train$survived))
prop.table(table(titanic.test$survived))

# 2) 각 모델에 동일한 평가방법 적용
fitControl <- trainControl(method='repeatedcv', number=10, repeats=3)
# 머신러닝 알고리즘별 최적 모수를 찾기 위한 학습방법 사전 설정

# 예측 모델 작성_1 ::: RandomForest
# install.packages('e1071')
library(e1071)
rf_fit <- train(survived ~ ., data=titanic.train,
                preProcess = c("pca"),
                method='rf', ntree=100, verbose=F, trControl=fitControl)

# 모델 선정
rf_fit
# mtry  Accuracy   Kappa    
# 2     0.7625214  0.4920897
# 4     0.7631868  0.4931313
# 7     0.7625031  0.49049404

# RandomForest 모델 평가
## 테스트셋에 적용 > 정확도 확인 > 모델 평가(분할표)

predict(rf_fit, newdata = titanic.test) %>% confusionMatrix(titanic.test$survived)
confusionMatrix(predict(rf_fit, newdata = titanic.test), titanic.test$survived)

# 예측 모델 작성_2 ::: DecisonTree

dt_fit <- train(survived ~ pclass + sex + age + sibsp + parch + fare + embarked, 
                data=titanic.train,
                method = 'ctree', na.action = na.pass,
                trControl = fitControl)
dt_fit

# DecisionTree 모델 평가
predict(dt_fit, newdata = titanic.test) %>% confusionMatrix(titanic.test$survived)
confusionMatrix(predict(dt_fit, newdata = titanic.test), titanic.test$survived)

# 모델링 심화연습
# glm, xgBoost, cart, RandomForest 적합 후 가장 성능이 뛰어나면서
# 가장 단순하고 이해하기 쉽고 운영환경에 배포하지 편한 것을 선정
# 판단기준으로 타당성검증(validation) 데이터를 활용

data("GermanCredit")
head(GermanCredit)
glimpse(GermanCredit)
# categorical var

# 1.2 데이터 분할, 훈련, 검증, 시험
## 훈련 및 검증/시험
in_train <- createDataPartition(GermanCredit$Class, p=c(.6, .4), list=F)
nrow(in_train) # 600
training <- GermanCredit[in_train, ]
validation_test <- GermanCredit[-in_train, ]

## 검증 및 시험
in_test <- createDataPartition(validation_test$Class, p=c(.5, .5), list=F)
validation <- validation_test[in_test, ]
length(validation) # 62

testing <- validation_test[-in_test, ]
length(testing) # 62

# 2.1 예측모형 공식(독일 신용평가 데이터)
# 데이터 탐색
colnames(training)
glimpse(training)
training$Class
levels(training$Class)
dim(training)

# training %>% 
#   select(-contains('Class')) -> credit_var
# dim(credit_var)

credit_var <- setdiff(colnames(training), list('Class'))
credit_var
credit_formula <- as.formula(paste('Class', paste(credit_var, collapse='+'), sep='~'))
credit_formula

# 2.2 예측모형 trainControl
ml_control <- trainControl(method='repeatedcv', number=10, repeats=5, 
                           sampling='up', verboseIter = F) # 클래스 불균형 방지

# 2.2.1 로지스틱 회귀
glm_m <- train(credit_formula, data=training, 
               method = 'glm', 
               family = binomial(link='logit'),
               trControl = ml_control)

# 2.2.2 의사결정나무(cart)
cart_m <- train(credit_formula, data=training, 
                method='rpart',
                trControl = trainControl(method='none', sampling='up'))

# 2.2.3 랜덤포레스트
rf_m <- train(credit_formula, data=training, method='rf', 
              trControl=ml_control)

# 2.2.4 gbm(Geometric Brownian Motion)
gbm_m <- train(credit_formula, data = training, method='gbm', 
               trControl= ml_control, verbose=F)

# 2.2.5 xgBoost
xgboost_m <- train(credit_formula, data = training, method = 'xgbLinear', 
                   trControl = ml_control)

# 2.3 모형 평가 아키텍쳐 with validation data
model_arch <- validation %>% # model_val
  mutate(GLM  = predict(glm_m, validation),
         CART = predict(cart_m, validation),
         RF = predict(rf_m, validation),
         XGB = predict(xgboost_m, validation),
         GBM = predict(gbm_m, validation))

model_arch[, c('Class', 'CART', 'RF', 'XGB', 'GBM')] # model_val

library(yardstick)

metrics(model_arch, truth=Class, estimate = CART)
metrics(model_arch, truth=Class, estimate = RF)
metrics(model_arch, truth=Class, estimate = GBM)
metrics(model_arch, truth=Class, estimate = XGB)

predict(glm_m, validation, type='prob')

# 성능에 차이가 있다고 하더라도 우연에 의한 것일 수 있으므로 paired t-test를 사용하여 모형간 유의성을 
# 검증한다. 만약 유의한 차이가 없다면 가장 단순한 모형을 선택한다.

diff_test <- resamples(list(GLM = glm_m, XGB = xgboost_m))
summary(diff_test)
diff(diff_test) %>% summary

# 2.4 모델 성능
testing_perf <- testing %>% # model_test
  mutate(CART = predict(cart_m, testing),
         RF = predict(rf_m, testing),
         XGB = predict(xgboost_m, testing),
         GBM = predict(gbm_m, testing))

metrics(testing_perf, truth=Class, estimate = CART)
metrics(testing_perf, truth=Class, estimate = RF)
metrics(testing_perf, truth=Class, estimate = GBM)
metrics(testing_perf, truth=Class, estimate = XGB)

# 예측값 저장
write.csv(predict(xgboost_m), './data/predict_result.csv')


