setwd("C:/Users/Daniel/ADP_performance_test")
getwd()

# install.packages('arules')
library(arules)
Packages <- c('plyr', 'dplyr', 'tidyverse', 'data.table', 'reshape2', 'caret', 'rpart', 'GGally', 'ROCR', 'party', 
              'randomForest', 'dummies', 'curl', 'gridExtra')

lapply(Packages, library, character.only=T)

# data loading----
tran <- read.csv('./data/transaction.csv', stringsAsFactors = F)
head(tran)

# invoice numbering----
group_number = (function(){i = 0; function() i <<- i+1 })()
# df %>% group_by(u,v) %>% mutate(label = group_number())
tran %>% 
  arrange(ymd, time, custid) %>%
  group_by(ymd, time, custid) %>%
  mutate(invoice_no = group_number()) -> tran_tmp

# making transaction_data for apriori----
transaction_data <- ddply(tran_tmp,c("invoice_no"),
                          function(tran_tmp)paste(tran_tmp$prod, collapse = ","))
head(transaction_data)  

# making transaction_data for apriori----
head(tran_tmp)
transaction_data <- ddply(tran_tmp,c("invoice_no"),
                          function(tran_tmp)paste(tran_tmp$prod, collapse = ","))
head(transaction_data)  

tran_tmp %>% 
  group_by(invoice_no) %>%
  summarise(paste(prod, collapse = ',')) -> transaction_data; head(transaction_data)

transaction_data[, 2]
write.table(transaction_data[, 2], './data/groceries_data.csv', 
            row.names = FALSE,  col.names=FALSE) 
## to writed data without header, use write.table() with col.names=FALSE

## apply apriori----
groceries <- read.transactions("./data/groceries_data.csv", sep = ",")
summary(groceries)
class(groceries)

# 처음 5개 거래 확인----
inspect(groceries[1:5])

# 식료품의 빈도 확인----
itemFrequency(groceries[, 1:50])

# 식료품의 빈도 시각화
# itemFrequencyPlot(groceries, support = 0.5)
itemFrequencyPlot(groceries, topN = 20)

# 처음 5개 거래에 대한 희소 매트릭스 시각화
windows()
image(groceries[1:4000])

# 100개 식료품의 무작위 샘플 시각화
image(sample(groceries, 100))

# ## 3단계 : 데이터에 대한 모델 훈련 ----
# library(arules)
# 
# # 기본 설정
# apriori(groceries)
# 
# # 규칙을 좀 더 학습히기 위해 지지도(support)와 신뢰도(confidence) 설정 변경
groceryrules <- apriori(groceries, parameter = list(support =0.00001, 
                                                    confidence = 0.00001,
                                                    minlen = 2))

summary(groceryrules)
inspect(groceryrules[1:5])

# groceryrules
# 
# ## 4단계 : 모델 성능 평가 ----
# # 식료품 연관 규칙의 요약
# summary(groceryrules)
# 
# # 처음 3개 규칙 확인
# inspect(groceryrules[1:3])
# 
# ## 5단계 : 모델 성능 향상 ----
# 
# # lift로 규칙 정렬
# inspect(sort(groceryrules, by = "lift")[1:5])
# 
# # 딸기류 아이템을 포함하는 규칙의 부분 규칙 찾기
# berryrules <- subset(groceryrules, items %in% "berries")
# inspect(berryrules)
# 
# # CSV 파일에 규칙 쓰기
# write(groceryrules, file = "groceryrules.csv",
#       sep = ",", quote = TRUE, row.names = FALSE)
# 
# # 규칙들을 데이터 프레임으로 변환
# groceryrules_df <- as(groceryrules, "data.frame")
# str(groceryrules_df)
