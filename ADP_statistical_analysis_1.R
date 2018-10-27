setwd("C:/Users/Daniel/ADP_performance_test")
getwd()

# 여러 개의 패키지를 한 번에 읽기
Packages <- c('plyr', 'dplyr','tidyverse', 'data.table', 'reshape2', 'caret', 'rpart', 'GGally', 'ROCR', 'party', 
              'randomForest')

lapply(Packages, library, character.only=T)

# 통계분석 기본
cor(iris$Sepal.Width, iris$Sepal.Length)
cor(iris[, 1:4]) # 비선형 :: mehthod = 'spearman'
cor.test(c(1,2,3,4,5), c(1,0,3,4,5), method='pearson')

# 하나의 연속변수
summary(mpg$hwy) # hwy : highway miles per gallon
mean(mpg$hwy)
median(mpg$hwy)
hist(mpg$hwy)
qqnorm(mpg$hwy)
qqline(mpg$hwy)
t.test(mpg$hwy, mu=22.9, alternative = 'greater')

# 하나의 범주형 변수 (성공-실패, binom.test)
# barplot() >> table(), prop.table() >> binom.test
data("GermanCredit")
glimpse(GermanCredit)
class(GermanCredit$Class)
x <- GermanCredit$Class
str(x); glimpse(x)
# 'Bad', 'Good'의 팩터를 0,1의 숫자형 데이터로 변환하는 방법

## 첫번째 factor(), as.numeric() 이용
x <- as.numeric(factor(x, levels = c('Bad', 'Good'), labels = c(0, 1))) - 1
class(x); x

## 두번째 ifelse() 이용
x <- GermanCredit$Class
x <- ifelse(GermanCredit$Class == "Bad", 0, 1)
class(x); x

# 반응변수 (y) ~ 설명변수 (x) 
# 연속형 변수 x, 연속형 변수 y: 선형모형이 적합----
# 산점도 -> 상관계수(선형 관계의 강도) -> 선형모형 -> 비선형 데이터에는 LOESS(국소회귀)

head(mpg)
plot(hwy ~ cty, data=mpg)
with(mpg, cor(cty, hwy))
with(mpg, cor(cty, hwy, method='spearman')) # 이상치가 많을 경우
model <- lm(hwy ~ cty, data=mpg)
summary(model) # 모델해석  t-value, p-value, adjusted R, 검정통계량

par(mfrow=c(2, 2))
plot(model) # 4가지 그래프 해석
par(mfrow=c(1,1))
# 가정 진단 :: 선형, 잔차의 분포 독립, 잔차의 분포 동일, 잔차의 정규분포 등 확인

yhat_model <-predict(model, newdata = data.frame(cty=c(10,20,20))); yhat_model # newdata as data.frame() in predict()
plot(mpg$hwy, mpg$cty)
abline(coef(model)) 

# with cars data--
head(cars)
plot(cars$speed, cars$dist)
abline(coef(model))

# 비선형(평활법) - LOESS (국소회귀)
str(mpg); unique(mpg$hwy)
with(mpg, plot(displ, hwy))
model <- loess(hwy ~ displ, data=mpg)
plot(model)
mpg %>% ggplot(aes(displ, hwy)) + geom_point() + geom_smooth()

# 범주형 변수 x, 연속형 변수 y: 분산분석 ANOVA속----
## 수면제 종류에 따른 수면량 증가, 차종에 따라 연비 차이, 혈압약과 혈압 감소량
## boxplot() -> lm(y(연속형 변수) ~ x(범주형 변수)) -> plot.lm() 잔차분포

mpg %>% ggplot(aes(class, hwy)) + geom_boxplot()
m <- lm(hwy ~ class, data=mpg)
summary(m) # 다른 집단과 유의하게 다른 평균 연비
# 연비의 총 변동량 중 차종으로 설명되는 비율, 모형의 적합도

yhat_m <- predict(m, newdata=data.frame(class='pickup')); yhat_m[1]
# 가정진단 : 잔차의 분포 독립, 잔차의 분포 동일 (잔차는 정규분포)
# 분포 독립성과 이상치 유무

# 연속형 변수 x, 범주형 변수 y(예 : 성공, 실패) :: 온도와 O링의 실패 여부 등----
# 산점도, 병렬상자 -> glm() 로지스틱 & binomial -> plot() 잔차분포, 모형 가정 확인
chall <- read_table("data/o-ring-erosion-only.data.txt", col_names = FALSE)
head(chall)
# chall :: the dataset of the Challenger locket's Orings
# 1. Number of O-rings at risk on a given flight :: o_ring_ct
# 2. Number experiencing thermal distress :: distress_ct
# 3. Launch temperature (degrees F) :: temperature
# 4. Leak-check pressure (psi) :: pressure
# 5. Temporal order of flight :: flight_order
colnames(chall) <- c('o_ring_ct', 'distress_ct', 'temperature', 'pressure', 'flight_order')
glimpse(chall); head(chall)
chall %>% ggplot(aes(temperature, distress_ct)) + geom_point()
chall %>% ggplot(aes(factor(distress_ct), temperature)) + geom_boxplot()

# 반응변수가 2차원 매트릭스(성공회수, 실패횟수)
model <- glm(cbind(distress_ct, o_ring_ct - distress_ct) ~ temperature, 
             data = chall, family='binomial') # 로지스틱 회귀
summary(model) # temperature의 효과 :: 온도 1도 상승할 때 로그 오즈비가 0.179 감소??
# 모형의 적합도 :: degree F가 1 줄었을때 deviance 충분히 감소 :: 적합


# 예측 :: 확률값
pred <- predict(model, data.frame(temperature=30), type='response'); pred

## 변수의 수가 많을 경우
# 1) 지도학습 :: 반응변수 예측(반응변수가 연속형변수 :: 회귀, 반응변수가 범주형변수 :: 분류)
# 2) 비지도학습 :: 변수들 간 / 관측치 간의 관계
# 군집 :: 관측치들 변수들간의 유사도로 그룹화
# 차원감소 :: 관측치들 간의 유사도를 이용하여 변수의 수를 감소

# 분류 : 주어진 입력변수로 범주형 반응변수 예측(성공/실패)
# 신용카드 사용자 -> 채무불이행 확률
# 투자할 회사 -> 투자 성공 확률
# 웹 방문자, 사이트, 방문시간 -> 광고 클릭 확률

#선형회귀 (연속형, 수치형 반응변수 예측)----
## 부동산 가격 예측 : 반응변수  medv
## medv :: Median value of owner-occupied homes in $1000's
read.table('./data/housing_data.csv') -> boston
names(boston) <- c('crim', 'zn', 'indus', 'chas', 'nox', 'rm', 'age', 'dis', 'rad',  
                   'tax', 'ptratio', 'black', 'lstat', 'medv')
glimpse(boston)
set.seed(2018)
boston %>% sample_n(100) -> training
model <- lm(medv~., data=training)
summary(model) # 연관성이 높은 변수 :: lstat, rm
# lstat :: % lower status of the population
# rm :: average number of rooms per dwelling

coef(model)
fitted(model)[1:4] # fitted value (171, 235, 31, 100 번째 데이터의 fitted value)
boston$medv[c(171, 235, 31, 100)] # observed value
residuals(model)[1:4] # observed value - fitted value
confint(model) # 신뢰구간 (정확한 의미 파악 필요)

# with ad_result.csv data----
m <- lm(install ~., data = data[,c('install', 'tvcm', 'magazine')])
summary(m) # 유의미한 변수는?

data <- read.csv('./data/ad_result.csv'); head(data)
data %>%
  select_if(is.numeric) %>%
  lm(install ~ ., .) -> m; summary(m)

coef(m)
fitted(m)
residuals(m)
confint(m)

# with cars data----
head(cars)
m <- lm(dist ~ speed, data=cars)
predict(m, newdata = data.frame(speed=3)) # 예측
predict(m, newdata = data.frame(speed=3), interval = 'confidence') # 평균적인 차량의 신뢰구간
predict(m, newdata = data.frame(speed=3), interval = 'prediction') # 특정속도 차량 한 대(오차)

# 다중선형회귀----

# Godd feature subsets contain features highly correlated with the classification, yet 
# uncorrelaetd to each other.

m <- lm(Sepal.Length ~ ., data=iris)
summary(m) # Speciesversicolor, Speciesvirginica
anova(m) # Species의 p-value 확인 / 의미파악하기

# using dummy variables----
levels(iris$Species)
library(dummies)
str(iris)
iris_dummy <- dummy.data.frame(iris, names=c('Species'), sep='_')
head(iris_dummy)
dim(iris_dummy)
iris_dummy
m <- lm(Sepal.Length ~ ., data=iris_dummy)
summary(m)
# Coefficients:
#   (Intercept)         Sepal.Width        Petal.Length         Petal.Width  
# 1.1478              0.4959              0.8292             -0.3152  
# Species_setosa  Species_versicolor   Species_virginica  
# 1.0235              0.2999                  NA  
# Why Speceis_virginica coef is NA

anova(m)

# 변수선택하기----
m <- lm(medv ~., data = boston)
m2 <- step(m, direction = 'both')
formula(m2)
# predict(m2, newdata=...)

write.csv(bio, './data/bio.csv', row.names = F)
bio <- read.csv('./data/bio.csv')
glimpse(bio)

# 전진소거법
step(lm(pemax~1, bio), 
     scope=list(lower=~1, upper=~age+weight+bmp+rv+frc+tlc), 
     direction = 'forward') # 변수를 weight와 bmp로 선택

# 후진소거법
step(lm(pemax~age+weight+bmp+rv+frc+tlc, bio), 
     direction = 'backward')

# 단계별 소거법
step(lm(pemax~1, bio), scope=list(lower=~1, upper=~age+weight+bmp+rv+frc+tlc), direction = 'both')


# ANOVA 분산분석, 모델간 비교(다변량)----
(full <- lm(dist ~ speed, data=cars))
(reduced <- lm(dist ~ 1, data = cars))
anova(reduced, full) # 1.49e-12 *** :: 이 두 모델간에는 유의한 차이가 있다.

# 상호작용 확인----
head(Orange)
range(Orange$age)
with(Orange, interaction.plot(age, Tree, circumference)) # age, Tree와의 상호작용이 circumference에 어떤 영향??

# 순서있는 범주형 -> 순서없는 범주형 :: 이유는??
Orange[, 'ftree'] <- factor(Orange[, 'Tree'], ordered=F)

m <- lm(circumference ~ ftree*age, data=Orange)
m1 <- lm(circumference ~ ftree + age, data=Orange)
anova(m, m1) # p-value = 9.402e-05 *** 이므로 두 모형 간에는 유의한 차이가 있음, 교호작용이 있음.

model_2 <- lm(medv ~ .^2, data=training) # 2차 상호작용 모형
summary(model_2) # 대부분의 변수가 유의하지 않음
length(coef(model_2)) # 많은 변수 : 과적합 / 해석 어려움

## 선형회귀 실전----
df_imdb <- read_csv('./data/imdb-5000-movie-dataset.zip')
summary(df_imdb)

df_imdb %>%
  ggplot(aes(content_rating)) + geom_bar()

df_imdb %>% 
  filter(content_rating %in% c('G', 'PG', 'PG-13', 'R')) %>%
  ggplot(aes(content_rating, imdb_score)) + 
  geom_point(alpha=.3, color = 'grey') + 
  geom_jitter() + 
  geom_boxplot(alpha=.7)

df_imdb %>%
  filter(content_rating %in% c("G", "PG", "PG-13", "R")) %>%
  ggplot(aes(imdb_score, fill=content_rating, linetype=content_rating)) + 
  geom_density(alpha=.3) # density plot..

summary(lm(imdb_score ~ content_rating, 
           data = df_imdb %>% filter(content_rating %in% c('G', 'PG', 'PG-13', 'R'))))
# 등급 집단간에 평점 평균이 통계적으로 유의한 차이가 있음

# '좋아요' 개수와 리뷰 평점 사이의 관계
df_imdb %>%
  ggplot(aes(movie_facebook_likes)) + geom_histogram()

df_imdb %>%
  ggplot(aes(movie_facebook_likes)) + geom_histogram() + scale_x_log10()

# 좋아요 개수와 스코어 간의 산점도
df_imdb %>%
  ggplot(aes(movie_facebook_likes, imdb_score)) + 
  geom_point() + 
  scale_x_log10() +
  geom_smooth()

# 2010년 이전과 이후의 좋아요 개수의 분포가 상이함을 확인 
df_imdb %>%
  ggplot(aes(as.factor(title_year), movie_facebook_likes)) +
  geom_boxplot() +
  scale_y_log10()

df_imdb %>%
  filter(title_year > 2010 & country == 'USA') %>%
  ggplot(aes(movie_facebook_likes, imdb_score)) +
  geom_point() +
  scale_x_log10() +
  geom_smooth()

# '좋아요' 100개 넘으면 두 변수 상관관계 높음
df_imdb_l00_more <- df_imdb %>%
  filter(title_year > 2010 & country == 'USA') %>%
  filter(movie_facebook_likes > 100)
head(df_imdb_l00_more); dim(df_imdb_l00_more)

cor(log10(df_imdb_l00_more$movie_facebook_likes), df_imdb_l00_more$imdb_score)
# '좋아요' 100개 넘으면 두 변수 상관관계 높음

## 로그변환 >> 선형모델의 해
m <- lm(imdb_score ~ log10(movie_facebook_likes), data=df_imdb_l00_more)
summary(m)

# 다중공선성 : (다변량)
ggpairs(mtcars[, c('mpg', 'disp', 'hp', 'wt', 'drat')]) # 독립변수간 높은 상관계수(.7 이상) 확인!
m <- lm(mpg ~ disp + hp + wt + drat, data=mtcars)
summary(m)
anova(m)

## 분류모델평가----
## with randomForest model applied with titanic dataset

read.csv('./data/titanic_preprocessed.csv') -> titanic; glimpse(titanic)
titanic$pclass <- as.factor(titanic$pclass)
titanic$sex <- as.factor(titanic$sex)
titanic$embarked <- as.factor(titanic$embarked)
glimpse(titanic)

# 1) 학습/평가 데이터셋 분리
nrow(titanic)
idx <- createDataPartition(titanic$survived, p=.8, list=F)
titanic.train <- titanic[idx, ]
titanic.test <- titanic[-idx, ]
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


predicted <- predict(rf_fit, newdata = titanic.test) # predicted values
actual <- titanic.test$survived # actual values
length(predicted)
length(actual)
xtabs(~ predicted + actual) # 분할표
predicted == actual
sum(predicted == actual)
length(actual)
sum(predicted == actual) / length(actual) # or nrow(actual in case / accuracy

## ROC 커브 및 AUC
library(ROCR)
# probs :: 분류 알고리즘이 예측한 점수(predicted probability)
# labels는 실제 분류true class가 저장된 벡터(actual vectors)

yhat_rf <- predict(rf_fit, newdata = titanic.test, type='prob')$survived ## the predicted prob of survived
head(yhat_rf)
y_obs <- titanic.test$survived # label :: actual vectors
head(y_obs)
# ROCR package를 적용하기 위해 prediction 를 생성해야 함
pred_rf <- prediction(yhat_rf, y_obs)
plot(performance(pred_rf, 'tpr', 'fpr')) # ROC curve
abline(0,1)
plot(performance(pred_rf, 'acc')) ## cutoff에 따른 accuracy 변화
performance(pred_rf, 'auc')@y.values[[1]] # auc

# 회귀모델 평가 RMSE :: 작을수록 정확----
## with lm model applied with boston housing data
read.table('./data/housing_data.csv') -> boston
names(boston) <- c('crim', 'zn', 'indus', 'chas', 'nox', 'rm', 'age', 'dis', 'rad',  
                   'tax', 'ptratio', 'black', 'lstat', 'medv')
glimpse(boston)

# splitting total dataset into training and validation dataset
idx <- createDataPartition(boston$medv, p=c(.6, .4), list=F)
boston.train <- boston[idx, ]; dim(boston.train)
boston.validation_test <- boston[-idx, ]; dim(boston.validation_test)

# splitting validation_test dataset into validation and test dataset 
idx <- createDataPartition(boston.validation_test$medv, p=c(.5, .5), list=F)
boston.validation <- boston.validation_test[idx, ]; dim(boston.validation)
boston.test <- boston.validation_test[-idx, ]; dim(boston.test)

m <- lm(medv~., data=boston.train)
summary(m)

# defining rmse function----
rmse <- function(y_obs, yhat) {
  sqrt(mean(y_obs - yhat)^2)
}

y_obs <- boston.validation$medv
yhat_m <- predict(m, newdata=boston.validation)

rmse(y_obs, yhat_m) # 작을 수록 정확한 모델

# 로지스틱 회귀 :: income이 <=50K", ">50K인지 여부 예측 with adult data
adult <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data",
                  sep=",",
                  header=F,
                  col.names=c("age", "type_employer", "fnlwgt", "education", 
                              "education_num","marital", "occupation", "relationship", "race","sex",
                              "capital_gain", "capital_loss", "hr_per_week","country", "income"),
                  fill=FALSE, 
                  strip.white=T)
glimpse(adult)
levels(adult$income)
adult$income <- factor(adult$income, levels=c("<=50K", ">50K" ), labels=c(0,1))

# splitting dataset into train and test dataset
idx <- createDataPartition(adult$income, p=c(.8, .2), list=F)

# trainig and test dataset defining
adult.train <- adult[idx, ]
adult.test <- adult[-idx, ]

# modeling with training dateset
m <- glm(income ~., data=adult.train, family=binomial)
summary(m)
fitted(m)[c(1:5, 51:55)] # 모델에 적합된 값 with probability
ifelse(fitted(m) >= .5, 1,0)[c(1:5, 51:55)] # 모델에 적합된 값 with 1(">50K"), 0("<=50K")

# predicting with test dataset
predict(m, newdata = adult.test, type='response')
pred <- ifelse(predict(m, newdata = adult.test, type='response') >= .5, 1, 0)

range(predict(m, newdata = adult.test, type='response'))
range(predict(m, newdata = adult.test))

# evaluation
yhat_m_class <- ifelse(predict(m, newdata = adult.test, type='response') >= .5, 1, 0)
y_obs <- adult.test$income

xtabs(~ yhat_m_class + y_obs)
sum(yhat_m_class == y_obs) / length(y_obs)
confusionMatrix(yhat_m_class, y_obs)

# ROC curve and AUC----
library(ROCR)
yhat_glm <- predict(m, newdata = adult.test, type='response'); yhat_glm
y_obs  <- adult.test$income; y_obs

pred <- prediction(yhat_glm, y_obs)
plot(performance(pred, 'tpr', 'fpr'))
abline(0,1)

plot(performance(pred, 'acc')) # cutoff 에 따른 accuracy의 변화
performance(pred, 'auc')@y.values[[1]] # auc

# 로지스틱 회귀 :: left or not인지 여부 예측 with hr data----
hr <- read_csv("./data/hr_comma_sep.csv")
colnames(hr) <- tolower(colnames(hr)); head(hr)

table(hr$left)
table(hr$sales)
table(hr$salary)

hr %>% 
  select(-sales, -salary) %>%
  sample_n(500) %>%
  ggpairs()

glimpse(hr)

x <- model.matrix(~. -left, data=hr) # 선형 모형 formualtion을 위한 문자열 문법

## data splitting into training, validation, test dataset----
set.seed(2018)
n <- nrow(hr)
idx <- 1:n
training_idx <- sample(idx, n*.6)
idx <- setdiff(idx, training_idx)
validate_idx <- sample(idx, n*.2)
test_idx <- setdiff(idx, validate_idx)

length(training_idx); length(validate_idx); length(test_idx)

training <- hr[training_idx, ]
validation <- hr[validate_idx, ]
test <- hr[test_idx, ]

## modeling----
hr_glm_full <- glm(left ~., data=training, family = binomial); summary(hr_glm_full)


## prediction accuracy visualization----
y_obs <- validation$left
yhat_glm <- predict(hr_glm_full, newdata=validation, type='response')

ggplot(data.frame(y_obs, yhat_glm), 
       aes(yhat_glm, fill=factor(y_obs))) + geom_density(alpha=.5)

## ROCR :: ROC curve and AUC----
library(ROCR)
pred_glm <- prediction(yhat_glm, y_obs) # pred :: probability
perf_glm <- performance(pred_glm, 'tpr', 'fpr')
plot(perf_glm, main='ROC curve for glm model') # ROC curve
performance(pred_glm, 'auc')@y.values[[1]] # auc

# 로지스틱 회귀 :: cancer or not인지 여부 예측 with breast cancer----
## data downloading with curl----
library(curl)
h <- new_handle(copypostfields = "moo=moomooo")
handle_setheaders(h,
                  "Content-Type" = "text/moo",
                  "Cache-Control" = "no-cache",
                  "User-Agent" = "A cow"
)

tmp <- tempfile()

curl_download('https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data', tmp, handle=h)
data <-read.csv(tmp, header=F)
feature_names <- c('radius', 'texture', 'perimeter', 'area', 'smoothness',
                   'compactness', 'concavity', 'concave_points', 'symmetry', 'fractal_dim')

names(data) <-
  c('id', 'class',
    paste0('mean_', feature_names),
    paste0('se_', feature_names),
    paste0('worst_', feature_names))

glimpse(data)

## define needed functions----
rmse <- function(y_obs, yhat){
  sqrt(mean((y_obs - yhat)^2))
}

binomial_deviance <- function(y_obs, yhat){
  epsilon = 0.0001
  yhat = ifelse(yhat < epsilon, epsilon, yhat)
  yhat = ifelse(yhat > 1-epsilon, 1-epsilon, yhat)
  a = ifelse(y_obs==0, 0, y_obs * log(y_obs/yhat))
  b = ifelse(y_obs==1, 0, (1-y_obs) * log((1-y_obs)/(1-yhat)))
  return(2*sum(a + b))
}

# cleansing data----
data <- data %>% select(-id)
data$class <- factor(ifelse(data$class == 'B', 0, 1))
glimpse(data)
summary(data)

# data EDA----
library(gridExtra)
p1 <- data %>% ggplot(aes(class)) + geom_bar()

p2 <- data %>% ggplot(aes(class, mean_concave_points)) +
  geom_jitter(col='gray') +
  geom_boxplot(alpha=.5)

p3 <- data %>% ggplot(aes(class, mean_radius)) +
  geom_jitter(col='gray') +
  geom_boxplot(alpha=.5)

p4 <- data %>% ggplot(aes(mean_concave_points, mean_radius)) +
  geom_jitter(col='gray') + geom_smooth()

grid.arrange(p1, p2, p3, p4, ncol=2)

# data splitting
set.seed(1606)
n <- nrow(data)
idx <- 1:n
training_idx <- sample(idx, n*.6)
idx <- setdiff(idx, training_idx)
validate_idx <- sample(idx, n*.2)
test_idx <- setdiff(idx, validate_idx)
training <- data[training_idx, ]
validation <- data[validate_idx, ]
test <- data[test_idx, ]

# logistic regression modeling----
data_lm_full <- glm(class ~., data=training, family=binomial)
summary(data_lm_full)
anova(data_lm_full)

# predict----
predict(data_lm_full, newdata = data[1:5, ], type='response') 
data[1:5, ]$class

# model validation----
library(ROCR)
y_obs <- as.numeric(as.character(validation$class)) # factor -> character -> numeric
y_obs
yhat_lm <- predict(data_lm_full, newdata=validation, type='response')
pred_lm <- prediction(yhat_lm, y_obs)
plot(performance(pred_lm, 'tpr', 'fpr'))
abline(0,1)
performance(pred_lm, 'auc')@y.values[[1]]
binomial_deviance(y_obs, yhat_lm)

