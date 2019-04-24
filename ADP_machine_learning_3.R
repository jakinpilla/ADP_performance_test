#' ---
#' title: "ADP ML_3"
#' author: "jakinpilla"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::github_document
#' ---


setwd("/home/insa/ADP_performance_test/")
getwd()

#+ setup, message = FALSE
Packages <- c('plyr', 'tidyverse', 'data.table', 'reshape2', 'caret', 'rpart', 'GGally',
              'ROCR', 'ranger', 'dummies', 'curl', 'gridExtra')
lapply(Packages, library, character.only = T)

library(HSAUR)
data('heptathlon')

head(heptathlon)
dim(heptathlon)
rownames(heptathlon)

#' score transform...
heptathlon %>%
  as_tibble() %>%
  mutate(hurdles = max(hurdles) - hurdles) %>%
  mutate(run200m = max(run200m) - run200m) %>%
  mutate(run800m = max(run800m) - run800m) -> heptathlon


cor(heptathlon)
ggpairs(heptathlon)

#' scale()----
scale(heptathlon)

iris %>%
  select_if(is.numeric) %>%
  scale() %>%
  as_tibble() %>%
  cbind(., iris$Species) -> iris_scaled

#' select vars----
library(mlbench)
data("Soybean")
dim(Soybean)

Soybean %>% as_tibble()

#' caret::nearZeroVar()----
nearZeroVar(Soybean)

Soybean %>%
  as_tibble() %>%
  select(-nearZeroVar(.)) -> mySoybean; mySoybean

#' caret::findCorrelation()----
data('Vehicle')
Vehicle %>% as_tibble() -> vehicle; vehicle

vehicle %>%
  select(-Class) %>%
  cor() %>%
  findCorrelation() -> high_cor; high_cor # default =.9

vehicle %>%
  select(-Class) %>%
  cor() %>%
  findCorrelation(cutoff = .7) -> high_cor; high_cor

vehicle %>%
  select(-high_cor) -> vehicle_filered; vehicle_filered

#' PCA ==> K-means Clustering----
heptathlon %>%
  select(-score) %>%
  prcomp(scale = T) -> h.pca; h.pca

summary(h.pca)

screeplot(h.pca, type ='lines')

#' first and second components...
h.pca
h.pca$rotation[, 1:2] # What is principal components...
h.pca$x # principal components per persons...

biplot(h.pca, cex=.7)

library(ykmeans)

h <- data.frame(h.pca$x)

head(h)

keys <- names(h); keys

km <- ykmeans(h, 
              keys, # variable names...
              'PC1', # target names...
              3:6 # number of cluster...
)

km

table(km$cluster)
ggplot(km, 
       aes(x = PC1, y = PC2, col = as.factor(cluster))) +
  geom_point(size = 3)


head(h)
k <- kmeans(h[, 1:2], 5)

plot(h[, 1:2], col = k$cluster, pch = k$cluster, size =3)

k$cluster %>% as.factor() -> k$cluster

ggplot(h, aes(PC1, PC2, col = k$cluster)) + geom_point(size = 3)

#' PCA ==> SVM Classification----

#' Data Loading...
library(e1071)

iris %>%
  select(-Species) %>%
  prcomp(scale = T) -> ir.pca; ir.pca

ir.pca$x %>% as_tibble() -> ir; ir

ir %>%
  cbind(., species = iris$Species) %>% 
  as_tibble() -> iris_pca

iris_pca

#' Data Spliting...
idx <- caret::createDataPartition(iris_pca$species, p = c(.8, .2), list = F)
iris_pca_train <- iris_pca[idx, ]
dim(iris_pca_train)

iris_pca_test <-iris_pca[-idx, ]
dim(iris_pca_test)

#' Modeling...
m_svm <- svm(species ~ ., data = iris_pca_train)
m_svm

#' Predicting...
y_hat_svm <- predict(m_svm, newdata = iris_pca_test[, 1:4])

table(y_hat_svm)

#' Evaluating...
confusionMatrix(y_hat_svm, iris_pca_test$species)

#' Visualization....
y_hat_df <- data.frame(iris_pca_test[, 1:4], y_hat = y_hat_svm)

y_hat_df

ggplot(y_hat_df,
       aes(x = PC1, y = PC2, col = y_hat)) + geom_point(size = 3)

#' With caret style...
#'
#' setting fitControl...
fitControl = trainControl(method = 'repeatedcv', 
                          number = 10, 
                          repeats = 3,
                          verboseIter = T)

#' Model Training...
iris_svm <- train(species ~ ., 
                  data = iris_pca_train,
                  method = 'svmLinear',
                  trControl = fitControl)

#' Predict with test dataset...
y_hat_caret_svm <- predict(iris_svm, iris_pca_test[, 1:4])

y_hat_caret_svm

#' Evaluating...
confusionMatrix(y_hat_caret_svm, iris_pca_test$species)

#' Visualization...
cbind(iris_pca_test[, 1:4], 
      species = y_hat_caret_svm) -> iris_hat_data

ggplot(iris_hat_data, aes(PC1, PC2, col = species)) + geom_point(size = 1)

#' Hclust----
protein <- read_delim("./data/protein.txt", "\t", 
                      escape_double = F,
                      trim_ws = T)
glimpse(protein)

protein %>%
  select(-Country) %>%
  scale() %>%
  dist() -> d; d

h <- hclust(d, method = 'ward.D')
plot(h, labels= protein$Country)

#' Flexclust----
library(flexclust)

data("nutrient")

head(nutrient)

nutrient %>%
  scale() %>%
  dist() -> d

h.fit_average <- hclust(d, method = "average")

plot(h.fit_average, hang = 1, cex = .8, 
     main = "Average Linkage Clustering")


#' How many Clusters----
library(NbClust)

nutrient %>%
  scale() -> nutrient.scaled

nc <-NbClust(nutrient.scaled, distance = 'euclidean', 
             min.nc = 2, max.nc = 15,
             method = 'average')

par(mfrow =c(1, 1))
barplot(table(nc$Best.nc[1, ]),
        xlab = 'Number of Clusters',
        ylab = 'Number of Criteria',
        main = 'Number of Clusters Chosen by 26 criteria')


#' Results----
clusters <- cutree(h.fit_average, k = 5)

table(clusters)


#' Visualization -----------------------------------------------------------
plot(h.fit_average,
     hang = -1, 
     cex = .8,
     main = 'Average Linkage Clustering with 5 Cluster Solution')

rect.hclust(h.fit_average, k=5)


#' MDS ---------------------------------------------------------------------

data("eurodist")
eurodist

loc <- cmdscale(eurodist)
x <- loc[, 1]
y <- loc[, 2]
plot(x, y, type = 'n', main = 'eurodist')
text(x, y, rownames(loc), cex = .8)
abline(v = 0, h = 0)


#' -------------------------------------------------------------------------

data("nutrient")
nutrient.scaled <- scale(nutrient)
nutrient.scaled %>% dist() -> d

d %>% cmdscale() -> loc
x <- loc[, 1]
y <- loc[, 2]
plot(x, y, type = 'n', main = 'Nutrient Distance')
text(x, y, rownames(loc), cex = .8)
abline(v = 0, h = 0)


#' NN ----------------------------------------------------------------------

library(nnet)
idx <- createDataPartition(iris$Species, p = c(.7, .3), list = F)
dim(idx)

iris_train <- iris[idx, ]
iris_train

iris_test <- iris[-idx, ]
iris_test

nn.iris <- nnet(Species ~., data = iris_train, size= 2, rang = 0, 
                decay = 5e-4, maxit = 200)

y_hat_nn <-predict(nn.iris, iris_test, type = 'class')
y_hat_nn %>% as.factor() -> y_hat_nn_fac

confusionMatrix(y_hat_nn_fac, iris_test$Species)


#' SVM ---------------------------------------------------------------------

library(e1071)
svm.iris <- svm(Species ~ ., data = iris_train, kernel = 'linear')

y_hat_svm <- predict(svm.iris, iris_test[, -5])

confusionMatrix(y_hat_svm, iris_test$Species)
