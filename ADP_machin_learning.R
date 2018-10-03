setwd("C:/Users/Daniel/ADP_performance_test")
getwd()

# install.packages('yardstick')
# install.packages('party')
# install.packages('randomForest')

# 여러 개의 패키지를 한 번에 읽기
Packages <- c('tidyverse', 'data.table', 'reshape2', 'caret', 'rpart', 'GGally', 'ROCR', 'party', 
              'randomForest', 'dplyr')
lapply(Packages, library, character.only=T)

# 예측할 변수 선정 > 데이터 분리 > 모델선택 > 학습 > 평가

# 예측할 변수를 'survived'로 선택
as.tibble(fread('./data/titanic3.csv', data.table = F)) -> titanic
glimpse(titanic)
titanic$pclass <- as.factor(titanic$pclass)
titanic$ticket <- as.character(titanic$ticket)
titanic$survived <- factor(titanic$survived, levels=c(0,1), labels=c('dead', 'survived'))
glimpse(titanic)

levels(titanic$survived)
prop.table(table(titanic$survived))

# 1) 학습/평가 데이터셋 분리
nrow(titanic)
idx <- createDataPartition(titanic$survived, p=.8)[[1]] # createDataPartition() 기본 리스트 반환

titanic.train <- titanic[idx, ]
titanic.test <- titanic[-idx, ]

prop.table(table(titanic.train$survived))
prop.table(table(titanic.test$survived))

