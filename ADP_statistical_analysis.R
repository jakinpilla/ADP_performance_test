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
## 두번째 ifelse() 이요
x <- GermanCredit$Class
x <- ifelse(GermanCredit$Class == "Bad", 0, 1)
class(x); x
## 세번째

GermanCredit$Class %>% 


