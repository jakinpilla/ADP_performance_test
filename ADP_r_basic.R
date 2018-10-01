setwd("C:/Users/Daniel/ADP_performance_test")
getwd()

# install.packages('yardstick')
# install.packages('party')
# install.packages('randomForest')

# 여러 개의 패키지를 한 번에 읽ㄱ
Packages <- c('tidyverse', 'data.table', 'reshape2', 'caret', 'rpart', 'GGally', 'ROCR', 'party', 
              'randomForest', 'dplyr')
lapply(Packages, library, character.only=T)


# large data set reading
fread('./data/titanic3.csv', data.table = F)
as.tibble(fread('./data/titanic3.csv', data.table = F)) -> titanic
tbl_df(fread('./data/titanic3.csv', data.table = F)) -> titanic

read.table('./data/housing_data.csv') -> boston
names(boston) <- c('crim', 'zn', 'indus', 'chas', 'nox', 'rm', 'age', 'dis', 'rad',  
                   'tax', 'ptratio', 'black', 'lstat', 'medv')
head(boston)
# crim :: per capita crime rate by town
# zn :: proportion of residential land zoned for lots over 25,000 sq.ft.
# indus :: proportion of non-retail business acres per town
# chas :: charles river dummy variable (=1 if tract bounds river; 0 otherwise)
# nox :: nitric oxides concentration (parts per 10 million)
# rm :: average number of rooms per dwelling
# age :: proportion of owner-occupied units built prior to 1940
# dis :: weighted distance to five Boston employment centers
# rad :: index of accessibility to radial highways
# tax :: full-value property-tax rate per $10,000
# ptratio :: pupil-teacher ratio by town
# black :: 1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town
# lstat :: % lower status of the population
# medv :: Median value of owner-occupied homes in $1000's

# 파일 읽기(다양한 형태의 파일 읽어오기)
read.table('./data/students.txt', sep='\t', header=F) -> student ; head(student) # 헤더가 없을 경우
d1 <- read.table("./data/student-mat.csv",sep=";",header=TRUE) ; head(d1) # 구분자가 ;일 경우

student <- read_delim("data/student.txt",  "\t", col_names = FALSE); student # 구분자가 tab이고 헤더가 없는 경우
student1 <- read_delim("data/student1.txt", "\t"); student1 # 구분자가 tab이고 헤더가 있는 경우
student2 <- read_delim("data/student2.txt", ";"); student2 # 구분자가 ;인 경우
## 특정 문자의 NA 처리 방법
student3 <- read_delim("data/student3.txt", "\t", na= '-') # -를 NA로 처리하고자 할 경우
## strip.white
student4 <- read_csv("data/student4.txt", col_names = FALSE); head(student4)
white_wine <- read.table('./data/winequality-white.csv', strip.white = F, sep=';', 
                         header=T); head(white_wine)

# na.rm=T
student3
mean(student3$키)
mean(student3$키, na.rm=T)

# 데이터 구경하기(glimpse, plot(numeric_var ~ factor_var, data))
glimpse(mpg)
summary(boston)
# windows()
plot(boston[, c('crim', 'zn', 'indus', 'chas', 'black', 'lstat', 'medv')])
ggpairs(boston[, c('crim', 'zn', 'indus', 'chas', 'black', 'lstat', 'medv')])
plot(boston$crim) # numeric인 경우, axis-x에는 index
plot(iris$Sepal.Length)
plot(iris$Species) # factor인 경우, axis-x에는 factor, y에는 갯수
plot(Species ~ Sepal.Length, data=iris) # lhs에 factor, rhs에 numeric일 경우, mosaic plot
plot(Sepal.Length ~ Species, data=iris) # lhs에 numeric, rhs에 factor일 경우, boxplot

# NA and outlier
df_imdb <- read_csv('./data/imdb-5000-movie-dataset.zip')
summary(df_imdb)
# 변수가 nuneric

## 특정 변수 내 NA 제거하기(dplyr::drop_na())
summary(df_imdb) ## 전체적인 NA 현황 파악
sum(is.na(df_imdb$gross)) ## 변수 별 NA 갯수 확인하기
df_imdb$gross[df_imdb$gross < 0] <- NA # 0보다 작은 값을 NA로 만들기
summary(df_imdb$budget) # budget 변수의 NA 제거 필요 확인
df_imdb %>%
  drop_na(budget) -> df_imdb_budget_na_drop ; summary(df_imdb_budget_na_drop)
nrow(df_imdb_budget_na_drop) ; range(df_imdb_budget_na_drop$budget) # budget 변수 내 NA 제거 확인

as.tibble(fread('./data/titanic3.csv', data.table = F)) -> titanic
summary(titanic)
titanic$cabin <- ifelse(titanic$cabin == "", NA, titanic$cabin) ## 빈칸을 NA로 만들기
titanic$cabin


## 특정 변수 내 oulier 찾고 제거하기(boxplot()$stat)
boxplot(df_imdb_budget_na_drop$budget, horizontal = T) # 이상치 제거 필요성 확인
boxplot(df_imdb_budget_na_drop$budget)$stat ## 
# [,1]
# [1,] 2.18e+02
# [2,] 6.00e+06
# [3,] 2.00e+07
# [4,] 4.40e+07
# [5,] 1.00e+08
# attr(,"class")
# 1 
# "integer" 

# 2.18e+02 ~ 1.00e+08를 벗어나면 이상치로 간주

df_imdb_budget_na_drop %>%
  filter(budget >= 2.18e+02 & budget <= 1.00e+08) -> df_imdb_budget_na_oulier_drop
boxplot(df_imdb_budget_na_oulier_drop$budget, horizontal = T)

nrow(df_imdb)
nrow(df_imdb_budget_na_oulier_drop)

# 데이터 sampling(sample_n(), sample_n(data, replace=T), sample_frac())
df_imdb %>% sample_n(10) # 비복원추출
df_imdb %>% sample_n(100, replace=T) # 복원추출
df_imdb %>% sample_frac(0.01, replace=T) # 복원추출

# 연속형 변수만 선택하기(dplyr::select_if)
mpg %>% select_if(is.numeric)

# 산점도행렬(GGally::ggpairs, 산점도, 상관계수를 한 번에 시각화)
pairs(iris[, 1:4])
iris %>% select_if(is.numeric) %>% ggpairs

# 상관계수 행렬
cor(iris[, 1:4])
round(cor(iris[, 1:4]), 2)
round(cor(iris[, 1:4]), 1)

# 데이터형 변환
head(titanic)
titanic$pclass <- as.factor(titanic$pclass)
titanic$ticket <- as.character(titanic$ticket)
titanic$survived <- factor(titanic$survived, levels=c(0,1), labels=c('dead', 'survived'))
glimpse(titanic)

# 데이터형 확인
class(titanic$embarked) # characeter
levels(titanic$embarked) # NULL
table(titanic$embarked)

titanic$embarked <- as.factor(titanic$embarked)
table(titanic$embarked)
table(titanic$embarked, useNA='always')

titanic$cabin <- as.factor(titanic$cabin)
table(titanic$cabin)
cabin <- as.data.frame(table(titanic$cabin)); names(cabin) <- c('room', 'count')
head(cabin)
ggplot(cabin[-1, ], aes(x=room, y=count)) + geom_bar(stat='identity')

