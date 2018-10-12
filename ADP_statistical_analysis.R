setwd("C:/Users/Daniel/ADP_performance_test")
getwd()

# install.packages('yardstick')
# install.packages('party')
# install.packages('randomForest')

# 여러 개의 패키지를 한 번에 읽기
Packages <- c('tidyverse', 'data.table', 'reshape2', 'caret', 'rpart', 'GGally', 'ROCR', 'party', 
              'randomForest', 'dplyr')

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
plot(model) # 4가지 그래프 해석
opar <- par(mfrow=c(2,2))
plot(model)
opar <- par(mfrow=c(1,1))
# 가정 진단 :: 선형, 잔차의 분포 독립, 잔차의 분포 동일, 잔차의 정규분포 등 확인

pred <-predict(model, newdata = data.frame(cty=c(10,20,20)))
pred

plot(mpg$hwy, mpg$cty)
abline(coef(model)) 

# with cars data----
head(cars)
plot(cars$speed, cars$dist)
abline(coef(model))

# 비선형(평활법) - LOESS (국소회귀)
str(mpg); unique(mpg$hwy)
model <- loess(hwy ~ displ, data=mpg)
plot(model)
mpg %>% ggplot(aes(displ, hwy)) + geom_point() + geom_smooth()

# 범주형 변수 x, 연ㅅ형 변수 y: 분산분석 ANOVA속----
## 수면제 종류에 따른 수면량 증가, 차종에 따라 연비 차이, 혈압약과 혈압 감소량
## boxplot() -> lm(y(연속형 변수) ~ x(범주형 변수)) -> plot.lm() 잔차분포

mpg %>% ggplot(aes(class, hwy)) + geom_boxplot()
model <- lm(hwy ~ class, data=mpg)
summary(model) # 다른 집단과 유의하게 다른 평균 연비
# 연비의 총 변동량 중 차종으로 설명되는 비율, 모형의 적합도

pred <- predict(model, newdata=data.frame(class='pickup'))
# 가정진단 : 잔차의 분포 독립, 잔차의 분포 동일 (잔차는 정규분포)
# 분포 독립성과 이상치 유무

# 연속형 변수 x, 범주형 변수 y(예 : 성공, 실패) :: 온도와 O링의 실패 여부 등----
# 산점도, 병렬상자 -> glm() 로지스틱 & binomial -> plot() 잔차분포, 모형 가정 확인
chall <- read_table("data/o-ring-erosion-only.data.txt",
                    +                     col_names = FALSE)
head(chall)
# chall :: the dataset of the Challenger locket's Orings
# 1. Number of O-rings at risk on a given flight :: o_ring_ct
# 2. Number experiencing thermal distress :: ring_ct
# 3. Launch temperature (degrees F) :: temperature
# 4. Leak-check pressure (psi) :: pressure
# 5. Temporal order of flight :: flight_order
colnames(chall) <- c('o_ring_ct', 'distress_ct', 'temperature', 'pressure', 'flight_order')
glimpse(chall)
chall %>% ggplot(aes(temperature, distress_ct)) + geom_point()
chall %>% ggplot(aes(factor(distress_ct), temperature)) + geom_boxplot()

# 반응변수가 2차원 매트릭스(성공회수, 실패횟수)
model <- glm(cbind(distress_ct, o_ring_ct - distress_ct) ~ temperature, 
             data = chall, family='binomial')
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

boston %>% sample_n(100) -> training
model <- lm(medv~., data=training)
summary(model) # 연관성이 높은 변수 :: lstat, rm
# lstat :: % lower status of the population
# rm :: average number of rooms per dwelling

coef(model)
fitted(model)[1:4] # fitted value (80, 401, 274, 251 번째 데이터의 fitted value)
boston$medv[c(80, 401, 274, 251)] # observed value
residuals(model)[1:4] # observed value - fitted value
confint(model) # 신뢰구간 (정확한 의미 파악 필요)

# with ad_result.csv data----
data <- read.csv('./data/ad_result.csv')
m <- lm(install ~., data = data[,c('install', 'tvcm', 'magazine')])
summary(m) # 유의미한 변수는?

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

# ANOVA 분산분석, 모델간 비교(다변량)----
(full <- lm(dist ~ speed, data=cars))
(reduced <- lm(dist ~ 1, data = cars))
anova(reduced, full) # 1.49e-12 *** :: 이 두 모델간에는 유의한 차이가 있다.

# 상호작용 확인----
head(Orange)
with(Orange, interaction.plot(age, Tree, circumference)) # age, Tree와의 상호작용이 circumference에 어떤 영향??

# 순서있는 범주형 -> 순서없는 범주형 :: 이유는??
Orange[, 'ftree'] <- factor(Orange[, 'Tree'], ordered=F)

m <- lm(circumference ~ ftree*age, data=Orange)
m1 <- lm(circumference ~ ftree + age, data=Orange)
anova(m, m1)

model_2 <- lm(medv ~ .^2, data=training) # 2차 상호작용 모형
summary(model_2) # 대부분의 변수가 유의하지 않음
length(coef(model_2)) # 많은 변수 : 과적합 / 해석 어려움

## 선형회귀 실전
df_imdb <- read_csv('./data/imdb-5000-movie-dataset.zip')
summary(df_imdb)

df_imdb %>%
  ggplot(aes(content_rating)) + geom_bar()
df_imdb %>% 
  filter(content_rating %in% c('G', 'PG', 'PG-13', 'R')) %>%
  ggplot(aes(content_rating, imdb_score)) + geom_boxplot()

summary(lm(imdb_score ~ content_rating, 
           data = df_imdb %>% filter(content_rating %in% c('G', 'PG', 'PG-13', 'R'))))
# 등급 집단간에 평점 평균이 통계적으로 유의한 차이가 있음

# '좋아요' 개수와 리뷰 평점 사이의 관계
df_imdb %>%
  ggplot(aes(movie_facebook_likes)) + geom_histogram()

df_imdb %>%
  ggplot(aes(movie_facebook_likes)) + geom_histogram() + scale_x_log10()

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



















