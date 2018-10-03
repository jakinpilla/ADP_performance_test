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
















