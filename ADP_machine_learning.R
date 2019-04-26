#' ---
#' title: "ADP ML with transaction dataset"
#' author: "jakinpilla"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::github_document
#' ---

rm(list=ls()); gc()
getwd()

#+setup, include = FALSE
Packages <- c('tidyverse', 'data.table', 'reshape2', 'caret', 'rpart', 'GGally', 
              'ROCR', 'randomForest', 'ranger','e1071')
lapply(Packages, library, character.only=T)

#' Data Wraggling > Sectecting vars > Spilting Data > Modeling > Evaluating > Comparing > Predicting...
#' 
#' Date Loading----
data_with_gender <- read_csv('./data/data_total_with_gender_final.csv') 
data_with_gender %>%
  select(custid, gender) -> cust_gender

data_total <- read_csv('./data/data_total.csv') 
data_total %>% colnames()

cust_gender %>%
  left_join(data_total, by = "custid") -> data

data %>% colnames()

data %>%
  select_if(is.numeric) -> var_num

glimpse(var_num)

#' Scaling and Making gender var ------------------------------------------------
scale(var_num) -> scaled_var_num # return matrix
as_tibble(scaled_var_num) -> var_num_tibble; head(var_num_tibble)

data %>% 
  select(gender) %>%
  cbind(var_num_tibble) %>%
  as_tibble() -> data_with_sex.tibble

#' Changing gender var class into factor ------------------------------------------------
data_with_sex.tibble$gender  <- factor(data_with_sex.tibble$gender, 
                                       levels = c("f", "m"), labels = c(0, 1))

df <- data_with_sex.tibble
nrow(df)

#' Cleansing colname for ML with `janitor` package
library(janitor)
df <- clean_names(df)

df %>% glimpse()

colnames(df) <- colnames(df) %>% make.names()

#' ML ------------------------------------------------
#'
#' Data splitting ------------------------------------------------
idx <- caret::createDataPartition(df$gender, p=c(.6, .4), list=F); 
idx[1:10]; length(idx)

#' df.train data ------------------------------------------------
df.train <- df[idx, ]
df.valid.test <- df[-idx, ]

nrow(df.train)
nrow(df.valid.test)

dim(df.train)
head(df.train)

#' Validation data ------------------------------------------------
head(df.valid.test)
idx <- createDataPartition(df.valid.test$gender, p=c(.5, .5), list=F)
idx[1:5]; length(idx)
df.valid <- df.valid.test[idx, ]
df.test <- df.valid.test[-idx, ]

dim(df.valid); head(df.valid)
dim(df.test); head(df.test)

1255 + 417 + 417 
#' There are disappeared data which have NA values and so on -------------------
#' 
#' Model Training with df.train dataset ------------------------------------------------
#'
#' Setting fitControl ------------------------------------------------
fitControl <- trainControl(method='repeatedcv', 
                           number=10,
                           repeats = 3)

#' Models Fitting ------------------------------------------------
#'
#' GLM ------------------------------------------------
#' 

colnames(df.train) %>% make.names() -> colnames(df.train)
colnames(df.test) %>% make.names() -> colnames(df.test)

glm_m <- train(gender ~ ., data = df.train, 
               method = 'glm', 
               trControl = fitControl,
               family = binomial(link='logit'))

glm_m

#' CART ------------------------------------------------

cart_m <- train(gender ~ ., data = df.train, 
                method = 'rpart',
                trControl = fitControl)

cart_m

#' `The final value used for the model was cp = 0.006963788` mean?
#' 
#' RF ------------------------------------------------
rf_m <- train(gender ~ ., data = df.train, method='rf', 
              trControl=fitControl)
rf_m

#' Model Evaluating ------------------------------------------------
model_arch <- df.valid %>% # model_val
  mutate(GLM  = predict(glm_m, df.valid),
         CART = predict(cart_m, df.valid),
         RF = predict(rf_m, df.valid))

model_arch %>%
  select('gender', 'GLM', 'CART', 'RF') -> model_pred_result; model_pred_result

library(yardstick)

metrics(model_arch, truth=gender, estimate = GLM)
metrics(model_arch, truth=gender, estimate = CART)
metrics(model_arch, truth=gender, estimate = RF)

#' RF is winner...
varImp(rf_m)

#' Predict with winner model and test datset ------------------------------------------------
predict(rf_m, df.test, type='prob') %>% write_csv("./data/predicted_data.csv")

y_hat_rf <- predict(cart_m, df.test, type='prob')$`1`
y_hat_rf[1:10]
y_obs <- df.test$gender

#' ROC curve and AUC ------------------------------------------------
library(ROCR)

#' Prediction Objection :: Probability and Labels ------------------------------------------------
pred_rf <- prediction(y_hat_rf, y_obs) 

#' Performance Object :: Prediction Object, 'tpr', 'fpr' ------------------------------------------------
perf_rf <- performance(pred_rf, 'tpr', 'fpr')

#' ROC Curve ------------------------------------------------
plot(perf_rf, main='ROC curve for glm model') 
abline(0,1)

#' Instead ------------------------------------------------
library(caTools)
colAUC(y_hat_rf, y_obs, plotROC = T)

#' AUC ------------------------------------------------
performance(pred_rf, 'auc')@y.values[[1]] # auc

#' xgBoost ------------------------------------------------
#'
#' Evaluation with validation data ------------------------------------------------
model_arch <- df.valid %>% # model_val
  mutate(GLM  = predict(glm_m, df.valid),
         CART = predict(cart_m, df.valid),
         RF = predict(rf_m, df.valid))
         # XGB = predict(xgboost_m, df.valid))

model_arch %>%
  select(gender, GLM, CART, RF) -> model_pred_result # select(gender, GLM, CART, RF, XGB)

head(model_pred_result, 30)

library(yardstick)

metrics(model_arch, truth=gender, estimate = GLM)
metrics(model_arch, truth=gender, estimate = CART)
metrics(model_arch, truth=gender, estimate = RF)
# metrics(model_arch, truth=gender, estimate = XGB)

predict(rf_m, df.valid, type='prob') %>% head()

#' Model performance
df.test_perf <- df.test %>% # model_test
  mutate(GLM = predict(glm_m, df.test),
         CART = predict(cart_m, df.test),
         RF = predict(rf_m, df.test))
         # XGB = predict(xgboost_m, df.test))

metrics(df.test_perf, truth=gender, estimate = GLM)
metrics(df.test_perf, truth=gender, estimate = CART)
metrics(df.test_perf, truth=gender, estimate = RF)
# metrics(df.test_perf, truth=gender, estimate = XGB)

