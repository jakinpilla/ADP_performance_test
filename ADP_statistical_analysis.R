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

# 연속형 변수 x, 연속형 변수 y: 선형모형이 적합
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

# with cars data
head(cars)
plot(cars$speed, cars$dist)
abline(coef(model))

# 비선형(평활법) - LOESS (국소회귀)
str(mpg); unique(mpg$hwy)
model <- loess(hwy ~ displ, data=mpg)
plot(model)
mpg %>% ggplot(aes(displ, hwy)) + geom_point() + geom_smooth()

# 범주형 변수 x, 연ㅅ형 변수 y: 분산분석 ANOVA
## 수면제 종류에 따른 수면량 증가, 차종에 따라 연비 차이, 혈압약과 혈압 감소량
## boxplot() -> lm(y(연속형 변수) ~ x(범주형 변수)) -> plot.lm() 잔차분포

mpg %>% ggplot(aes(class, hwy)) + geom_boxplot()
model <- lm(hwy ~ class, data=mpg)
summary(model) # 다른 집단과 유의하게 다른 평균 연비
# 연비의 총 변동량 중 차종으로 설명되는 비율, 모형의 적합도

pred <- predict(model, newdata=data.frame(class='pickup'))
# 가정진단 : 잔차의 분포 독립, 잔차의 분포 동일 (잔차는 정규분포)
# 분포 독립성과 이상치 유무

# 연속형 변수 x, 범주형 변수 y(예 : 성공, 실패) :: 온도와 O링의 실패 여부 등
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




















