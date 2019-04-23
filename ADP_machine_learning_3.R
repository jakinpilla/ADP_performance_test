# PCA
# setwd("C:/Users/Daniel/ADP_performance_test")
getwd()

Packages <- c('plyr', 'dplyr', 'tidyverse', 'data.table', 'reshape2', 'caret', 'rpart', 'GGally', 
              'ROCR', 'randomForest', 'dummies', 'curl', 'gridExtra')
lapply(Packages, library, character.only=T)


# install.packages('HSAUR')
library(HSAUR)
data('heptathlon')

head(heptathlon)
glimpse(heptathlon)
rownames(heptathlon)

# 큰 수가 좋은 점수가 되도록 최대값에서 빼준다.----
heptathlon$hurdles <- max(heptathlon$hurdles) - heptathlon$hurdles
heptathlon$run200m <- max(heptathlon$run200m) - heptathlon$run200m
heptathlon$run800m <- max(heptathlon$run800m) - heptathlon$run800m

cor(heptathlon)
ggpairs(heptathlon) # 독립변수간 높은 상관계수 확인(.7 이상) -> dimension reduction

# 정규화 
scale(heptathlon)
cbind(as.data.frame(scale(iris[1:4])), iris$Species)

# 변수선택(모델링에 가장 적합한 변수)----
# install.packages('mlbench')
library(mlbench); data(Soybean)
glimpse(Soybean)

# 정보량이 0에 가까운 변수를 선택(분산이 0에 가까운...) caret :: nearZeroVar()
nearZeroVar(Soybean)
mySoybean <- Soybean[, -nearZeroVar(Soybean)] # 0에 가까운 분산 (정보량 적음)

# 변수간 상관관계가 높은 것을 삭제 caret :: findCorrelation() 
data("Vehicle"); head(Vehicle)

Vehicle %>%
  select(-Class) %>% 
  cor() %>%
  findCorrelation() -> high_cor; high_cor

Vehicle %>%
  select(-Class) %>% 
  cor() %>%
  findCorrelation(cutoff = .7) -> high_cor; high_cor # default = .9

Vehicle %>% 
  select(-high_cor) -> vehicle_filtered

# 주성분분석 선택, 평가
h <- prcomp(heptathlon[,-8], scale=T)
summary(h)
screeplot(h, type='lines')
h # 구성요소들(주성분 계수)
h$rotation[, 1:2] # 제1주성분, 제2주성분 각각 해석
h$x[, 1:2] # 선수별 주성분에 따른 점수

biplot(h, cex=.7) 
# 가까운 거리, 방향일수록 변수들의 상관성이 높다.
# 각 개체가 특정 변수에 가깝게 위치할수록 해당 변수와 관련이 크다.

# 주성분 분석 -> kmeans

# ykmeans()----
# install.packages('ykmeans')
library(ykmeans)
h <- data.frame(h$x); head(h) # 주성분에 따른 점수
keys <- names(h); keys
km <- ykmeans(h, keys, 'PC1', 3:6) # 'PC1"을 중심으로 군집수를 3개 ~ 6개로 하여 군집분석을 한다.
km
table(km$cluster)
ggplot(km,
       aes(x= PC1, y=PC2, col=as.factor(cluster), shape=as.factor(cluster))) + geom_point(size=5)

head(h)
k <- kmeans(h[, 1:2], 5)
plot(h[, 1:2], col=k$cluster, pch=k$cluster, size=5)

# 주성분 분석 -> SVM----
library(e1071)
ir <- prcomp(iris[, 1:4], scale=T)
summary(ir) # 변수 갯수 선택
screeplot(ir, type='lines') # 변수 갯수 선택
plot(ir, type='l') # 변수 갯수 선택 / 변수를 2개로 선택
ir$rotation[, 1:2] # 주성분 1, 2의 요소들 확인
as.data.frame(ir$x)[1:3, ] # transformed data format

iris_pca <- cbind(as.data.frame(ir$x), Species = iris$Species) # ir$x는 아직 matrix이므로...
dim(iris_pca); head(iris_pca)
glimpse(iris_pca)

# data spliting----
idx <- createDataPartition(iris_pca$Species, p=c(.8, .2), list=F)
iris_pca_train <- iris_pca[idx, ] ; dim(iris_pca_train)
iris_pca_test <- iris_pca[-idx, ] ; dim(iris_pca_test)

# modeling----
m_svm <- svm(Species ~ ., data = iris_pca_train)

# predict(with newdata=...)---
yhat_svm <- predict(m_svm, newdata=iris_pca_test[, 1:4]); table(yhat_svm)

# evaluating----
predict(m_svm, newdata=iris_pca_test[, 1:4]) %>% confusionMatrix(iris_pca_test$Species)

# visualise----
yhat_df <- data.frame(yhat=yhat_svm, iris_pca_test[, 1:4]); head(yhat_df)
ggplot(yhat_df,
       aes(x=PC1, y=PC2, col=yhat, shape=yhat)) + geom_point(size=2)

# with caret things----
fitControl <- trainControl(method='repeatedcv', number = 10, repeats=3)
iris_svm <- train(Species ~ ., data=iris_pca_train, method='svmLinear',
                  trControl = fitControl)
predict(iris_svm, newdata = iris_pca_test) %>% confusionMatrix(iris_pca_test$Species)

data.frame(yhat = predict(iris_svm, newdata = iris_pca_test) , iris_pca_test[, 1:4]) -> df
head(df)
ggplot(df, aes(PC1, PC2, col=yhat, shape=yhat)) + geom_point(size=2)
ir$rotation[, 1:2]
ggplot(df, aes(PC1, PC2, col=yhat, shape=yhat)) + geom_point(size=2) +
  xlab('PC1=0.52*Sepal.Length - 0.27*Sepal.Width + 0.58*Petal.Length  + 0.56*Petal.Width') +
  ylab('PC2=-0.38*Sepal.Length - 0.92*Sepal.Width - 0.02*Petal.Length  - 0.07*Petal.Width')


# hclust()----
protein <- read_delim("./data/protein.txt",  "\t", escape_double = FALSE, trim_ws = TRUE)
glimpse(protein)
vars <- colnames(protein)[-1]
pmatrix <- scale(protein[, vars]) # matrix class
d <- dist(pmatrix, method='euclidean')
h <- hclust(d, method='ward.D')
plot(h, labels = protein$Country)

princ <- prcomp(pmatrix)
project <- predict(princ, newdata=pmatrix)[, 1:2]


# hclust()----
# install.packages('flexclust')
library(flexclust)
data(nutrient, package='flexclust'); head(nutrient)
nutrient.scaled <- scale(nutrient)

d <- dist(nutrient.scaled)
fit.average = hclust(d, method='average')

plot(fit.average, hang=-1, cex=.8, main='Average Linkage Clustering')

# how many clusters?----
library(NbClust)
nc <- NbClust(nutrient.scaled, distance='euclidean', min.nc = 2, max.nc=15, 
              method='average')

par(mfrow=c(1, 1))

barplot(table(nc$Best.n[1, ]), 
        xlab = 'Number of Clusters',
        ylab = 'Number of Criteria',
        main = 'Number of Clusters Chosen by 26 criteria') # n=5

# 최종결과획득----
clusters <- cutree(fit.average, k=5)
table(clusters) 

# 분석결과시각화----
plot(fit.average, hang=-1, cex=.8, 
     main = 'Average Linkage Clustering with 5 Cluster Solution')
rect.hclust(fit.average, k=5)

# 다차원척도법----
## with eurodist dataset----
data(eurodist)
eurodist      
loc <- cmdscale(eurodist)
x <- loc[,1]
y <- loc[,2]
plot(x, y, type='n', main='eurodist')
text(x, y, rownames(loc), cex=.8)
abline(v=0, h=0)

## with nutrient dataset----
data(nutrient, package='flexclust'); head(nutrient)
nutrient.scaled <- scale(nutrient)
d <- dist(nutrient.scaled)
position <- cmdscale(d)

plot(position)
text(position, as.character(rownames(nutrient)), cex=.5)
abline(v=0, h=0)

# 인공신경망----
library(nnet)
idx <- createDataPartition(iris$Species, p=c(.7, .3), list=F);dim(idx)
iris_train <- iris[idx, ]; dim(iris_train)
iris_test <- iris[-idx, ]; dim(iris_test)

head(iris_train)
nn.iris <- nnet(Species ~., data=iris_train, size=2, rang=0, decay=5e-4, maxit=200)
nn_pred <- predict(nn.iris, iris_test, type='class')
nn_pred

# SVM----
library(e1071)
svm.iris <- svm(Species ~., data=iris_train, kernel='linear')
yhat_svm <- predict(svm.iris, iris_test[, -5])
y_obs <- iris_test[, 5]

table(yhat_svm, y_obs)
confusionMatrix(yhat_svm, y_obs)




