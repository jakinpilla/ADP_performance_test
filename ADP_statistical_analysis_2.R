#' ---
#' title: "ADP STAT_2"
#' author: "jakinpilla"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::github_document
#' ---

setwd("C:/Users/Daniel/ADP_performance_test")
getwd()

#' 여러 개의 패키지를 한 번에 읽기
#' 
#+setup, include = F
Packages <- c('plyr', 'dplyr', 'tidyverse', 'data.table', 'reshape2', 'caret', 'rpart', 'GGally', 'ROCR', 'party', 'randomForest', 'dummies', 'curl', 'gridExtra')

lapply(Packages, library, character.only=T)

#' logistic regression with spambase---------------------------------------------
#' 
#' data loading(ham or spam)----
# h <- new_handle(copypostfields = "moo=moomooo")
# handle_setheaders(h,
#                   "Content-Type" = "text/moo",
#                   "Cache-Control" = "no-cache",
#                   "User-Agent" = "A cow"
# )
# 
# tmp <- tempfile()
# 
# curl_download('https://archive.ics.uci.edu/ml/machine-learning-databases/spambase/spambase.data', tmp, handle=h)
# data <-read.csv(tmp, header=F)
# head(data)
# feature_names <- c('radius', 'texture', 'perimeter', 'area', 'smoothness',
#                    'compactness', 'concavity', 'concave_points', 'symmetry', 'fractal_dim')
# 
# names(data) <-
#   c('word_freq_make', 'word_freq_address', 'word_freq_all', 'word_freq_3d', 'word_freq_our',
#     'word_freq_over', 'word_freq_remove', 'word_freq_internet', 'word_freq_order', 'word_freq_mail',
#     'word_freq_receive', 'word_freq_will', 'word_freq_people', 'word_freq_report', 'word_freq_addresses',
#     'word_freq_free', 'word_freq_business', 'word_freq_email', 'word_freq_you', 'word_freq_credit',
#     'word_freq_your', 'word_freq_font', 'word_freq_000', 'word_freq_money', 'word_freq_hp',
#     'word_freq_hpl', 'word_freq_george', 'word_freq_650', 'word_freq_lab', 'word_freq_labs',
#     'word_freq_telnet', 'word_freq_857', 'word_freq_data', 'word_freq_415', 'word_freq_85',
#     'word_freq_technology', 'word_freq_1999', 'word_freq_parts', 'word_freq_pm', 'word_freq_direct',
#     'word_freq_cs', 'word_freq_meeting', 'word_freq_original', 'word_freq_project', 'word_freq_re',
#     'word_freq_edu', 'word_freq_table', 'word_freq_conference', 'char_freq_;', 'char_freq_(',
#     'char_freq_[', 'char_freq_!', 'char_freq_$', 'char_freq_#', 'capital_run_length_average',
#     'capital_run_length_longest', 'capital_run_length_total',
#     # 'spam'
#     'class'
#   )

# 
# data %>% 
#   write_csv("./data/word_counts_spam.csv")

data <- read_csv("./data/word_counts_spam.csv")
data$class <- factor(data$class); glimpse(data)
dim(data)


tmp <- as.data.frame(cor(data[,-58], as.numeric(data$class))); 
tmp
tmp <- tmp %>% rename(cor=V1) 
tmp$var <- rownames(tmp)
head(tmp)

#' What words are strongly related with spam mail? -----------------------------
tmp %>%
  ggplot(aes(reorder(var, cor), cor)) +
  geom_point() +
  coord_flip()

#' Visualizing data -------------------------------------------------------------
p1 <- data %>% ggplot(aes(class)) + geom_bar()

p2 <- data %>% ggplot(aes(class, `char_freq_$`)) +
  geom_jitter(col='gray') +
  geom_boxplot(alpha=.5) +
  scale_y_sqrt()

p3 <- data %>% ggplot(aes(`char_freq_$`, group=class, fill=class)) +
  geom_density(alpha=.5) +
  scale_x_sqrt() + scale_y_sqrt()

p4 <- data %>% ggplot(aes(class, capital_run_length_longest)) +
  geom_jitter(col='gray') +
  geom_boxplot(alpha=.5) +
  scale_y_log10()

grid.arrange(p1, p2, p3, p4, ncol=2)

#' Punc character change -------------------------------------------------------------
old_names <- names(data)
old_names
new_names <- make.names(names(data), unique=T) # make.names() : change punc character...
new_names
cbind(old_names, new_names) [old_names != new_names,] # beautiful coding...
names(data) <- new_names

#' Splitting dataset  -------------------------------------------------------------
set.seed(2019)
n <- nrow(data)
idx <- 1:n
training_idx <- sample(idx, n * .6)
idx <- setdiff(idx, training_idx)
validate_idx <- sample(idx, n * .2)
test_idx <- setdiff(idx, validate_idx)
training <- data[training_idx, ]
validation <- data[validate_idx, ]
test <- data[test_idx, ]

training_idx %>% length()
validate_idx %>% length()
test_idx %>% length()

#' Logistic regression  -------------------------------------------------------------
data_lm_full <- glm(class ~., data=training, family = binomial)
summary(data_lm_full)
predict(data_lm_full, newdata = data[1:5, ], type='response')

#' Predict ----------------------------------------------------------------------
head(training)
head(test)
y_hat_lm_train <- predict(data_lm_full, newdata=training, type='response')
y_hat_lm_test <- predict(data_lm_full, newdata=test, type='response')

#' Model Evalidation -----------------------------------------------------------------
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

validation$class %>% as.character() %>% as.numeric() -> y_obs
y_hat_lm <- predict(data_lm_full, newdata=validation, type='response')

# make prediction object...
pred_lm <- prediction(y_hat_lm, y_obs)

# make performance object...
perf_lm <- performance(pred_lm, 'tpr', 'fpr')

# ROC curve...
plot(perf_lm)
abline(0, 1, col = "red")

# AUC value...
performance(pred_lm, 'auc')@y.values[[1]]
binomial_deviance(y_obs, y_hat_lm)

# confusion matrix...
range(y_hat_lm_test)
y_hat_lm <- as.factor(ifelse(y_hat_lm_test > .5, 1, 0))
y_obs <- as.factor(test$class)
table(y_obs, y_hat_lm) -> cm; print(cm)

confusionMatrix(y_hat_lm, y_obs)

#' Multi-logistic regression with iris dataset & nnet package...-----------------

#' 다항 로지스틱 회귀 : 예측하고자 하는 분류 여러개 :: multinom ------------------
library(nnet)
m_nnet <- multinom(Species ~., data=iris)
head(fitted(m_nnet))
predict(m_nnet, newdata = iris[c(1,51,101), ], type='class')
predict(m_nnet, newdata = iris[c(1,51,101), ], type='probs') # default :: probs

#' Accuracy----
y_hat_nnet <- predict(m_nnet, newdata=iris) 
sum(y_hat_nnet == iris$Species) / length(y_hat_nnet)
xtabs(~ y_hat_nnet + iris$Species)

#' DT and RF modeling with titanic dataset---------------------------------------

#' Loading titanic_preprocessed dataset
read_csv('./data/titanic_preprocessed.csv') %>%
  mutate(survived = as.factor(survived))-> titanic; head(titanic)
titanic$pclass <- as.factor(titanic$pclass)
titanic$sex <- as.factor(titanic$sex)
titanic$embarked <- as.factor(titanic$embarked)
glimpse(titanic)

#' DT with rpart package----
#' 
#' 수량형 변수를 팩터로 변환, 의사결정나무에 적합
#' 
#' Splitting dataset----
set.seed(2019)
n <- nrow(titanic)
idx <- 1:n
training_idx <- sample(idx, n * .6)
idx <- setdiff(idx, training_idx)
validate_idx <- sample(idx, n * .2)
test_idx <- setdiff(idx, validate_idx)
titanic.train <- titanic[training_idx, ]
titanic.validation <- titanic[validate_idx, ]
titanic.test <- titanic[test_idx, ]

#' Plotting----
m_dt <- rpart(survived ~ pclass + sex + age + sibsp + parch + fare + embarked, data=titanic.train)
plot(m_dt)
text(m_dt, cex=.8)

#' The complexity parameter (cp) ....
#' 
#' CP is used to control the size of the decision tree and to select the optimal tree size.
printcp(m_dt)

summary(m_dt)

#' Var Importance----
varImp(m_dt) %>%
  mutate(var.name = rownames(.)) %>% 
  arrange(desc(Overall))

#' Model Evaluation----
y_hat_dt <- predict(m_dt, newdata = titanic.validation); head(y_hat_dt)
ifelse(as.data.frame(predict(m_dt, newdata = titanic.validation))$survived > .5, 'survived', 'dead') -> y_hat_dt
y_hat_dt <- as.factor(y_hat_dt)
length(y_hat_dt)
levels(y_hat_dt)
y_obs <- titanic.validation$survived; head(y_obs)
y_obs <- as.factor(y_obs)
length(y_obs)
levels(y_obs)

confusionMatrix(y_hat_dt, y_obs)


library(ROCR)
as.numeric(as.factor(titanic.validation$survived))
as.numeric(as.factor(titanic.validation$survived)) - 1
y_obs <- as.numeric(y_obs) - 1

predict(m_dt, newdata=titanic.validation) %>% class() # matrix

y_hat_dt <- as.data.frame(predict(m_dt, newdata=titanic.validation))$survived
pred_dt <- prediction(y_hat_dt, y_obs)
plot(performance(pred_dt, 'tpr', 'fpr'))
abline(0,1, col = "red")
performance(pred_dt, 'auc')@y.values[[1]] 

#' DT with ctree package :: 조건부 추론나무-----
#' 
#' Modeling ----
m_ctree <- ctree(survived ~ pclass + sex + age + sibsp + parch + fare + embarked, data=titanic.train)

#' Plotting----
plot(m_ctree)
levels(titanic.train$survived)

#' Model Evaluating with ConfusionMatrix and ROC curve----
#' 
#' ConfusionMatirx...
y_hat_ctree <- predict(m_ctree, newdata = titanic.validation, type = 'response')
head(y_hat_ctree)

y_obs <- titanic.validation$survived
head(y_obs)

confusionMatrix(y_hat_ctree, y_obs)

#' ROC curve...
library(ROCR)
y_hat_ctree_1 <- as.numeric(y_hat_ctree ) - 1 
y_hat_ctree_1 %>% enframe() %>% select(-name) -> y_hat_ctree_2

y_obs_1 <- as.numeric(y_obs) - 1

# make prediction object...
pred_ctree <- prediction(y_hat_ctree_2, y_obs_1)

# make performance object...
perf_ctree <- performance(pred_ctree, "tpr", "fpr")

# ROC curve...
plot(perf_ctree)
abline(0, 1, col = "red")

# AUC...
performance(pred_ctree, 'auc')@y.values[[1]] # 0.7901751

#' RandomForest ----------------------------------------------------------------
#' 
#' Modeling...
#+ warning = FALSE
fitControl <- trainControl(method='repeatedcv', number=10, repeats=3)
rf_fit <- train(survived ~ ., data=titanic.train,
                preProcess = c("pca"),
                method='rf', ntree=100, verbose=F, trControl=fitControl)

#' Model Evaluating with confusion matrix and ROC curve----
#' 
#' ConfusionMatrix...
y_hat_rf <- predict(rf_fit, titanic.validation)
y_obs <- titanic.validation$survived

confusionMatrix(y_hat_rf, y_obs)

#' ROC curve...
library(ROCR)
y_hat_rf_1 <- as.numeric(y_hat_rf ) - 1 
y_hat_rf_1 %>% enframe() %>% select(-name) -> y_hat_rf_2

y_obs_1 <- as.numeric(y_obs) - 1

# make predicton object...
pred_rf <- prediction(y_hat_rf_2, y_obs_1)

# make performance object...
perf_rf <- performance(pred_rf, "tpr", "fpr")

plot(perf_rf)
abline(0, 1, col = "red")

# AUC...
performance(pred_rf, 'auc')@y.values[[1]] 


#' Leave or not with HR dataset---------------------------------------------------
#' 
#' Data Loading....
hr <- read_csv("data/hr_comma_sep.csv")
colnames(hr) <- tolower(colnames(hr)); glimpse(hr)

table(hr$left)
table(hr$sales)
table(hr$salary)

#' Splitting dataset----
set.seed(2019)
n <- nrow(hr)
idx <- 1:n
training_idx <- sample(idx, n * .6)
idx <- setdiff(idx, training_idx)
validate_idx <- sample(idx, n * .2)
test_idx <- setdiff(idx, validate_idx)
hr.train <- hr[training_idx, ]
hr.validation <- hr[validate_idx, ]
hr.test <- hr[test_idx, ]

#' RF modeling----
#' 
#' Factorising var for RF model....
rf <- randomForest(as.factor(left) ~., hr.train %>% 
                     mutate(salary = as.factor(salary),
                            sales = as.factor(sales)))


#' What are varables important?....

importance(rf) # not arranged...
importance(rf)[, 1]
data.frame(var=rownames(importance(rf)), gini_desc=importance(rf)[, 1]) -> var_imp_df
var_imp_df %>%
  arrange(desc(gini_desc))

varImpPlot(rf) ## 변수 중요도를 한 번에 시각화 가능

#' How many trees are need to make model?
plot(rf) # MSE variance to decide how many trees

#' Evaluaing----
#' 
#' Prediction obj...
y_hat_rf <- predict(rf, newdata=hr.validation %>% 
                      mutate(left = as.factor(left), # 자료형을 training과 같게
                             salary = as.factor(salary), # 자료형을 training과 같게
                             sales = as.factor(sales)), type='prob')[, '1'] # left 인 경우만 선택

y_hat_rf[1:5]

y_obs <- hr.validation$left; y_obs[1:5]
pred_rf <- prediction(y_hat_rf, y_obs)

#' Performance obj...
perf_rf <- performance(pred_rf, 'tpr', 'fpr')

#' ROC curve...
plot(perf_rf)
abline(0, 1, col = "red")

#' AUC...
plot(performance(pred_rf, 'acc'))
performance(pred_rf, 'auc')@y.values[[1]]

#' GLM---------------------------------------------------------------------------
#' 
#' Modeling...
hr_glm_full <- glm(left ~., data=hr.train, family = binomial); summary(hr_glm_full)

#' Evaluatiing----
#' 
#' Prediction obj...
y_hat_glm <- predict(hr_glm_full, newdata = hr.validation,  type='response')
y_obs <- hr.validation$left
pred_glm <- prediction(y_hat_glm, y_obs)

#' Performaec obj...
perf_glm <- performance(pred_glm, 'tpr', 'fpr')

#' ROC curve...
plot(perf_glm)
abline(0, 1, col = "red")

#' AUC...
plot(performance(pred_glm, 'acc'))
performance(pred_glm, 'auc')@y.values[[1]]

#' Comparison betweem perf_glm, perf_rf----
plot(perf_glm)
plot(perf_rf, add=T, col='blue')
abline(0, 1, col='red')

#' 'AUC' data.frame----
data.frame(method=c('glm', 'rf'), 
           auc = c(performance(pred_glm, 'auc')@y.values[[1]], 
                   performance(pred_rf, 'auc')@y.values[[1]]))

