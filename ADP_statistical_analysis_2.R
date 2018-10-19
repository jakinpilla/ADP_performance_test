setwd("C:/Users/Daniel/ADP_performance_test")
getwd()

# install.packages('yardstick')
# install.packages('party')
# install.packages('randomForest')

# 여러 개의 패키지를 한 번에 읽기
Packages <- c('plyr', 'dplyr', 'tidyverse', 'data.table', 'reshape2', 'caret', 'rpart', 'GGally', 'ROCR', 'party', 
              'randomForest', 'dummies', 'curl', 'gridExtra')

lapply(Packages, library, character.only=T)

# logistic trgression with spambase----
# data loading----
h <- new_handle(copypostfields = "moo=moomooo")
handle_setheaders(h,
                  "Content-Type" = "text/moo",
                  "Cache-Control" = "no-cache",
                  "User-Agent" = "A cow"
)

tmp <- tempfile()

curl_download('https://archive.ics.uci.edu/ml/machine-learning-databases/spambase/spambase.data', tmp, handle=h)
data <-read.csv(tmp, header=F)
head(data)
feature_names <- c('radius', 'texture', 'perimeter', 'area', 'smoothness',
                   'compactness', 'concavity', 'concave_points', 'symmetry', 'fractal_dim')

names(data) <-
  c('word_freq_make', 'word_freq_address', 'word_freq_all', 'word_freq_3d', 'word_freq_our',
    'word_freq_over', 'word_freq_remove', 'word_freq_internet', 'word_freq_order', 'word_freq_mail',
    'word_freq_receive', 'word_freq_will', 'word_freq_people', 'word_freq_report', 'word_freq_addresses',
    'word_freq_free', 'word_freq_business', 'word_freq_email', 'word_freq_you', 'word_freq_credit',
    'word_freq_your', 'word_freq_font', 'word_freq_000', 'word_freq_money', 'word_freq_hp',
    'word_freq_hpl', 'word_freq_george', 'word_freq_650', 'word_freq_lab', 'word_freq_labs',
    'word_freq_telnet', 'word_freq_857', 'word_freq_data', 'word_freq_415', 'word_freq_85',
    'word_freq_technology', 'word_freq_1999', 'word_freq_parts', 'word_freq_pm', 'word_freq_direct',
    'word_freq_cs', 'word_freq_meeting', 'word_freq_original', 'word_freq_project', 'word_freq_re',
    'word_freq_edu', 'word_freq_table', 'word_freq_conference', 'char_freq_;', 'char_freq_(',
    'char_freq_[', 'char_freq_!', 'char_freq_$', 'char_freq_#', 'capital_run_length_average',
    'capital_run_length_longest', 'capital_run_length_total',
    # 'spam'
    'class'
  )

head(data)
names(data)[58] <- 'class'
data$class <- factor(data$class)
glimpse(data)

tmp <- as.data.frame(cor(data[,-58], as.numeric(data$class))); head(tmp)
tmp <- tmp %>% rename(cor=V1)
tmp$var <- rownames(tmp)
head(tmp)
tmp %>%
  ggplot(aes(reorder(var, cor), cor)) +
  geom_point() +
  coord_flip()

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


# 변수명의 특수문자 처리----
old_names <- names(data)
old_names
new_names <- make.names(names(data), unique=T) # 특수문자를 숫자로 바꾸어줌
new_names
cbind(old_names, new_names) [old_names != new_names,]

names(data) <- new_names

# splitting dataset----
set.seed(1606)
n <- nrow(data)
idx <- 1:n
training_idx <- sample(idx, n * .6)
idx <- setdiff(idx, training_idx)
validate_idx <- sample(idx, n * .2)
test_idx <- setdiff(idx, validate_idx)
training <- data[training_idx, ]
validation <- data[validate_idx, ]
test <- data[test_idx, ]

# logistic regression----
data_lm_full <- glm(class ~., data=training, family = binomial)
summary(data_lm_full)
predict(data_lm_full, newdata = data[1:5, ], type='response')

# predict----
head(training)
head(test)
yhat_lm_train <- predict(data_lm_full, newdata=training, type='response')
yhat_lm_test <- predict(data_lm_full, newdata=test, type='response')

# model evalidation----
mse <- function(yi, yhat_i){
  sqrt(mean((yi - yhat_i)^2))
}

binomial_deviance <- function(y_obs, yhat){
  epsilon = 0.0001
  yhat = ifelse(yhat < epsilon, epsilon, yhat)
  yhat = ifelse(yhat > 1-epsilon, 1-epsilon, yhat)
  a = ifelse(y_obs==0, 0, y_obs * log(y_obs/yhat))
  b = ifelse(y_obs==1, 0, (1-y_obs) * log((1-y_obs)/(1-yhat)))
  return(2*sum(a + b))
}
y_obs <- as.numeric(as.character(validation$class))
yhat_lm <- predict(data_lm_full, newdata=validation, type='response')
pred_lm <- prediction(yhat_lm, y_obs)
plot(performance(pred_lm, 'tpr', 'fpr'))
abline(0,1)
performance(pred_lm, 'auc')@y.values[[1]]
binomial_deviance(y_obs, yhat_lm)

# confusion matrix
range(yhat_lm_test)
yhat_lm <- as.factor(ifelse(yhat_lm_test > .5, 1, 0))
y_obs <- as.factor(test$class)
table(y_obs, yhat_lm) -> cm; print(cm)

confusionMatrix(yhat_lm, y_obs)


# 다항 로지스틱 회귀 : 예측하고자 하는 분류 여러개 :: multinom
library(nnet)
m <- multinom(Species ~., data=iris)
head(fitted(m))
predict(m, newdata = iris[c(1,51,101), ], type='class')
predict(m, newdata = iris[c(1,51,101), ], type='probs') # default :: probs

# accuracy----
yhat <- predict(m, newdata=iris) 
sum(yhat == iris$Species) / length(yhat)
xtabs(~ yhat + iris$Species)

# 의사결정나무----
# 수량형 변수를 팩터로 변환, 의사결정나무에 적합
# loading titanic_preprocessed dataset
read.csv('./data/titanic_preprocessed.csv') -> titanic; head(titanic)
glimpse(titanic)

# splitting dataset----
set.seed(2018)
n <- nrow(titanic)
idx <- 1:n
training_idx <- sample(idx, n * .6)
idx <- setdiff(idx, training_idx)
validate_idx <- sample(idx, n * .2)
test_idx <- setdiff(idx, validate_idx)
titanic.train <- titanic[training_idx, ]
titanic.validation <- titanic[validate_idx, ]
titanic.test <- titanic[test_idx, ]

# plotting----
m <- rpart(survived ~ pclass + sex + age + sibsp + parch + fare + embarked, data=titanic.train)
plot(m)
text(m, cex=.8)

printcp(m)
summary(m)

# var importance----
varImp(m)

# model evaluation----
yhat <- predict(m, newdata = titanic.validation); head(yhat)
ifelse(as.data.frame(predict(m, newdata = titanic.validation))$survived > .5, 'survived', 'dead') -> yhat
yhat <- as.factor(yhat)
length(yhat)
levels(yhat)
y_obs <- titanic.validation$survived; head(y_obs)
length(y_obs)
levels(y_obs)

confusionMatrix(yhat, y_obs)

library(ROCR)
y_obs <- as.numeric(y_obs) - 1
yhat_lm <- as.data.frame(predict(m, newdata=titanic.validation))$survived
pred_lm <- prediction(yhat_lm, y_obs)
plot(performance(pred_lm, 'tpr', 'fpr'))
abline(0,1)
performance(pred_lm, 'auc')@y.values[[1]]

# ctree :: 조건부 추론나무
# 과적합, 변수선택 편중 문제 해결
m <- ctree(Species ~ ., data=iris)
plot(m)
levels(iris$Species) # 가려서 안 보일때
yhat <- predict(m, newdata = iris, type = 'response')
head(yhat)

# randomForest----
# 변수 >> 팩터형 >> 분류!

# survived or not with titanic dataset
fitControl <- trainControl(method='repeatedcv', number=10, repeats=3)
rf_fit <- train(survived ~ ., data=titanic.train,
                preProcess = c("pca"),
                method='rf', ntree=100, verbose=F, trControl=fitControl)

# left or not with hr dataset
hr <- read_csv("data/hr_comma_sep.csv")
colnames(hr) <- tolower(colnames(hr)); glimpse(hr)

table(hr$left)
table(hr$sales)
table(hr$salary)

# splitting dataset----
set.seed(2018)
n <- nrow(hr)
idx <- 1:n
training_idx <- sample(idx, n * .6)
idx <- setdiff(idx, training_idx)
validate_idx <- sample(idx, n * .2)
test_idx <- setdiff(idx, validate_idx)
hr.train <- hr[training_idx, ]
hr.validation <- hr[validate_idx, ]
hr.test <- hr[test_idx, ]

# factorising var fot rf model
rf <- randomForest(as.factor(left) ~., hr.train %>% 
                     mutate(salary = as.factor(salary),
                            sales = as.factor(sales)))

# select rf model----
importance(rf)
importance(rf)[, 1]
data.frame(var=rownames(importance(rf)), gini_desc=importance(rf)[, 1]) -> var_imp_df
var_imp_df %>%
  arrange(desc(gini_desc))

varImpPlot(rf) ## 변수 중요도를 한 번에 시각화 가능

plot(rf) # MSE variance to decide how many trees

# predict----
yhat <- predict(rf, newdata=hr.validation %>% 
                  mutate(left = as.factor(left), # 자료혐을 training과 같게
                         salary = as.factor(salary), # 자료혐을 training과 같게
                         sales = as.factor(sales)), type='prob')[, '1'] # left 인 경우만 선택하기 위해

yhat[1:5]

# evaluaing----
y_obs <- hr.validation$left; y_obs[1:5]
pred <- prediction(yhat, y_obs)
plot(performance(pred, 'tpr', 'fpr'))
abline(0, 1)
plot(performance(pred, 'acc'))
performance(pred, 'auc')@y.values[[1]]


# ROC 및 AUC 모델 비교 평가----

## glm model
hr_glm_full <- glm(left ~., data=hr.train, family = binomial); summary(hr_glm_full)
yhat_glm <- predict(hr_glm_full, newdata = hr.validation,  type='response')
y_obs <- hr.validation$left
pred_glm <- prediction(yhat_glm, y_obs)
perf_glm <- performance(pred_glm, 'tpr', 'fpr')

## rf model
yhat_rf <- predict(rf, newdata=hr.validation %>% 
                     mutate(left = as.factor(left), # 자료혐을 training과 같게
                            salary = as.factor(salary), # 자료혐을 training과 같게
                            sales = as.factor(sales)), type='prob')[, '1']

pred_rf <- prediction(yhat_rf, as.factor(y_obs))
perf_rf <- performance(pred_rf, 'tpr', 'fpr')

## comparison betweem perf_glm, perf_rm
plot(perf_glm)
plot(perf_rf, add=T, col='red')
abline(0, 1, col='blue')

### 'auc' daya.frame
data.frame(method=c('glm', 'rf'), 
           auc = c(performance(pred_glm, 'auc')@y.values[[1]], 
                   performance(pred_rf, 'auc')@y.values[[1]]))

# 






























