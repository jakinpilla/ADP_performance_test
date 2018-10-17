setwd("C:/Users/Daniel/ADP_performance_test")
getwd()

# install.packages('arules')
library(arules)
Packages <- c('plyr', 'dplyr', 'tidyverse', 'data.table', 'reshape2', 'caret', 'rpart', 'GGally', 'ROCR', 'party', 
              'randomForest', 'dummies', 'curl', 'gridExtra')

lapply(Packages, library, character.only=T)

# data loading----
tran <- read.csv('./data/transaction.csv', stringsAsFactors = F)
head(tran); dim(tran) ## 27,993건의 거래내역

# invoice numbering----
i=0
group_number = (function(){i = 0; function() i <<- i+1 })()
# df %>% group_by(u,v) %>% mutate(label = group_number())
tran %>% 
  arrange(ymd, time, custid) %>%
  group_by(ymd, time, custid) %>%
  mutate(basket_id = group_number()) %>%
  as.data.frame() %>% # to avoid adding grouped var...
  select(basket_id, prod) -> tran_basket; head(tran_basket)

# make basket.transaction and basket.transaction sparse format---- 
basket.transaction <- split(tran_basket$prod, tran_basket$basket_id)
basket.transaction[1:5]

basket.transaction <- as(basket.transaction, 'transactions')
basket.transaction

# 처음 5개 거래 확인----
inspect(basket.transaction[1:5])

# 식료품의 빈도 확인----
itemFrequency(basket.transaction[, 1:5])

# 식료품의 빈도 시각화
itemFrequencyPlot(basket.transaction, topN = 20)
itemFrequencyPlot(basket.transaction, support = .05, 
                  main = 'item frequency plot above support 1%')

# 처음 5개 거래에 대한 희소 매트릭스 시각화
# windows()
image(basket.transaction[1:100])

# 100개 식료품의 무작위 샘플 시각화
image(sample(basket.transaction, 200))

 
# 규칙을 좀 더 학습히기 위해 지지도(support)와 신뢰도(confidence) 설정 변경
groceryrules <- apriori(basket.transaction) # rule :: 0ro6>?
groceryrules <- apriori(basket.transaction, parameter = list(support =0.0001, 
                                                    confidence = 0.0001,
                                                    minlen = 2))

summary(groceryrules)
inspect(groceryrules[1:5])

# arrrange by support----
inspect(sort(groceryrules, by='support')[1:5])

# arrange by lift----
inspect(sort(groceryrules, by = "lift")[1:5])
 
# `harddrinks` 아이템을 포함하는 규칙의 부분 규칙 찾기
harddrinks_rules <- subset(groceryrules, items %in% "harddrinks")
inspect(harddrinks_rules[1:5])

# item based rules finding----
rule_interest_lhs <- subset(groceryrules, lhs %in% c('icecream'))
inspect(rule_interest_lhs[1:5])


# %in%, %pin%, %ain%----
## %in% :: 적어도 하나의 제품이라도 존재하면 연관규칙을 indexing 
## %pin% :: 부분일치만 하더라도 연관규칙을 indexing
## %ain% :: 완전일치하는 연관규칙만을 indexing

rule_interest_lhs <- subset(groceryrules, lhs %in% c('bread', 'milk'))
inspect(rule_interest_lhs[1:5])

rule_interest_lhs <- subset(groceryrules, lhs %pin% c('mi'))
inspect(rule_interest_lhs[1:5]) ## rule 안에 "mi"란 문자가 들어가 있으면 indexing

rule_interest_lhs <- subset(groceryrules, lhs %ain% c('cooky/cakes', 'fermentedmilk'))
inspect(rule_interest_lhs[1:5])

# write groceryrules(write())----
write(harddrinks_rules, file = "./data/harddrinks_rules.csv", sep = ",", 
      quote = TRUE, row.names = FALSE)

# convert rules as datafame----
harddrinks_rules_df <- as(harddrinks_rules, "data.frame")
str(harddrinks_rules_df)
head(harddrinks_rules_df)

# Visualization arules :: arulesViz
# install.packages('arulesViz')
# install.packages('viridisLite')
library(viridisLite)
library(arulesViz)

# plotting rules----
groceryrules <- apriori(basket.transaction, parameter = list(support =0.0001, 
                                                             confidence = 0.0001,
                                                             minlen = 3))
plot(groceryrules)
plot(sort(groceryrules, by='support')[1:20], method='grouped')

windows()
plot(groceryrules, method='graph', control=list(type='items'),
     vertex.label.cex = .7, 
     edge.arrow.size= .3,
     edge.arrow.size= 2)

plot(groceryrules[1:20], method='graph', control=list(type='items'))
plot(groceryrules[21:40], method='graph', control=list(type='items'))
plot(groceryrules[41:60], method='graph', control=list(type='items'))

sort(groceryrules, by = "lift") -> groceryrules_by_lift

plot(groceryrules_by_lift, method='graph', control=list(type='items'),
     vertex.label.cex = .7, 
     edge.arrow.size= .5,
     edge.arrow.size= 5)

plot(groceryrules_by_lift[1:20], method='graph', 
     control=list(type='items'),
     vertex.label.cex = .7, 
     edge.color = 'dark',
     edge.width = 10,
     edge.arrow.size= .5,
     edge.arrow.width = 2,
     edge.arrow.size= 5)

plot(groceryrules_by_lift[21:40], method='graph', control=list(type='items'))
plot(groceryrules_by_lift[41:60], method='graph', control=list(type='items'))

# ----
# LoL Champoion Dataset :: sample-data.csv
# load data and transder it into transactions format for apriori----
library(arules)
df <- read.csv('./data/sample-data-1.csv'); str(df)
table(df$id) # 총 18명의 챔피언 플레이 정보
head(df)
dim(df)
length(unique(df$names))

rioter.list <- split(df$names, df$id)
rioter.transaction <- as(rioter.list, 'transactions')
rioter.transaction

# generate rules----
rules <- apriori(rioter.transaction)
summary(rules)

rule.list <- as.data.frame(inspect(rules)); head(rule.list)
data.frame(lhs = rule.list$lhs, 
           rhs = rule.list$rhs,
           support = rule.list$support,
           confidence = rule.list$confidence, 
           lift = rule.list$lift,
           count = rule.list$count) -> rule_df
colnames(rule_df)
glimpse(rule_df)

rule_df %>%
  arrange(-lift)

# itemFrequencyPlot()----
itemFrequencyPlot(rioter.transaction, topN=10)

# image()----
image(rioter.transaction[1:10])

# inspect rules----
inspect(rules[1:5])

# lift로 규칙 정렬----
inspect(sort(rules, by = "lift")[1:5])

# `자이라` 를 포함하는 규칙의 부분 규칙 찾기
zaira_rules <- subset(rules, items %in% "자이라")
inspect(zaira_rules)

# generate rules with condition list----
rules <- apriori(rioter.transaction, parameter = list(supp=.001,
                                                      conf=.8))
summary(rules)

itemFrequencyPlot(basket.transaction, topN = 20)

# 처음 5개 거래에 대한 희소 매트릭스 시각화
# windows()
image(basket.transaction[1:4000])

# 100개 식료품의 무작위 샘플 시각화
image(sample(basket.transaction, 100))





































