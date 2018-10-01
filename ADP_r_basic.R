setwd("C:/Users/Daniel/ADP_performance_test")
getwd()

# install.packages('yardstick')
# install.packages('party')
install.packages('randomForest')

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


