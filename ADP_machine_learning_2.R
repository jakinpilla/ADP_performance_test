#' ---
#' title: "ADP ML_2_apriori"
#' author: "jakinpilla"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::github_document
#' ---

#+setup, include = FALSE
# setwd("C:/Users/Daniel/ADP_performance_test")

rm(list=ls()); gc()
setwd("/home/insa/ADP_performance_test")
getwd()

Packages <- c('tidyverse', 'data.table', 'reshape2', 'caret', 'rpart', 'GGally', 'ROCR',
              'randomForest', 'dummies', 'curl', 'gridExtra', 'arules', 'arulesViz',
              'viridisLite')
lapply(Packages, library, character.only=T)

#' Data loading----
tran <- read.csv('./data/transaction.csv', stringsAsFactors = F)
head(tran); dim(tran) ## 27,993건의 거래내역

tran %>% 
  arrange(ymd, time, custid) %>%
  group_by(ymd, time, custid) %>%
  summarise(basket_prods = paste(prod, collapse = ", "))

tran %>% 
  arrange(ymd, time, custid) %>%
  group_by(ymd, time, custid) %>%
  summarise(basket_prods = paste(prod, collapse = ", ")) %>%
  rowid_to_column() %>%
  rename(invoice_no = rowid)

tran %>% 
  arrange(ymd, time, custid) %>%
  group_by(ymd, time, custid) %>%
  summarise(basket_prods = paste(prod, collapse = ", ")) %>%
  rowid_to_column() %>%
  rename(invoice_no = rowid) %>%
  ungroup() %>%
  select(invoice_no, basket_prods) 

tran %>% 
  arrange(ymd, time, custid) %>%
  group_by(ymd, time, custid) %>%
  summarise(basket_prods = paste(prod, collapse = ", ")) %>%
  rowid_to_column() %>%
  rename(invoice_no = rowid) %>%
  ungroup() %>%
  select(invoice_no, basket_prods) %>%
  rename(items = basket_prods) %>%
  write_csv("./data/market_basket_transactions.csv")
  
basket.transaction_1 <- read.transactions("./data/market_basket_transactions.csv",
                                        format = "single",
                                        cols = c(1, 2),
                                        header = T, # if not, there would be column name in transaction data
                                        sep = ",")

basket.transaction_1 # sparse matrix...
summary(basket.transaction_1)

# ?read.transactions()
  
#' 1~5 baskets -------------------------------------------------------------
inspect(basket.transaction_1[1:5])
inspect(basket.transaction_1[10]) 


#' Data loading----
tran <- read.csv('./data/transaction.csv', stringsAsFactors = F)
head(tran); dim(tran) ## 27,993건의 거래내역

#' Invoice numbering----
i=0
group_number = (function(){i = 0; function() i <<- i+1 })()
# df %>% group_by(u,v) %>% mutate(label = group_number())

tran %>%
  arrange(ymd, time, custid) %>%
  group_by(ymd, time, custid) %>%
  mutate(basket_id = group_number()) %>%
  as.data.frame() %>% # to avoid adding grouped var...
  select(basket_id, prod) -> tran_basket; head(tran_basket)

#' Make basket.transaction and basket.transaction sparse format----
basket.transaction <- split(tran_basket$prod, tran_basket$basket_id)
basket.transaction[1:5] # list class...

basket.transaction_2 <- as(basket.transaction, 'transactions')
basket.transaction_2 # transaction class...


#' 1~5 baskets -------------------------------------------------------------

inspect(basket.transaction_2[1:5])


#' Item Frequency :: itemFrequency()----------------------------------------------------------

itemFrequency(basket.transaction_2[, 1:5])


#' Item Frequency Plot :: itemFrequencyPlot() ------------------------------

itemFrequencyPlot(basket.transaction_2, topN = 20)

library(RColorBrewer)
itemFrequencyPlot(basket.transaction_2, topN = 20,
                  type = "absolute", 
                  col = brewer.pal(8, 'Pastel2'),
                  main = "Absolute Item Frequency Plot")

itemFrequencyPlot(basket.transaction, support = .01, 
                  main = 'item frequency plot above support 1%')

#' Visualize Sparse Marix ------------------------------
# windows()
image(basket.transaction_2[1:100]) # 100 transaction, 100 items...

#' 200 transaction, 100 items....
image(sample(basket.transaction_2, 200))

 
#' How to set support and confidence? ------------------------------
groceryrules <- apriori(basket.transaction) # default...

groceryrules <- apriori(basket.transaction, 
                        parameter = list(support = 0.0001, 
                                         confidence = 0.000001, 
                                         minlen = 2,
                                         maxlen = 10))

summary(groceryrules)
inspect(groceryrules[1:5])

#' Arrange by support ------------------------------
inspect(sort(groceryrules, by='support')[1:5])

#' Arrange by lift ------------------------------
inspect(sort(groceryrules, by = "lift")[1:5])
 
#' Find rules which contrain `harddrinks` item ------------------------------
harddrinks_rules <- subset(groceryrules, items %in% "harddrinks")
inspect(harddrinks_rules[1:5])

#' Item based rules finding ------------------------------
rule_interest_lhs <- subset(groceryrules, lhs %in% c('icecream'))
inspect(rule_interest_lhs[1:5])


#' %in%, %pin%, %ain% ------------------------------
# %in% :: at least one item rule matched rules' indexing 
# %pin% :: partially name character matched rules' indexing
# %ain% :: totallgy matched rules' indexing

rule_interest_lhs <- subset(groceryrules, lhs %in% c('bread', 'milk'))
inspect(rule_interest_lhs[1:5])

rule_interest_lhs <- subset(groceryrules, lhs %pin% c('mi')) # if "mi" character is contained in the rule, the rules are detected...
inspect(rule_interest_lhs[1:5]) 

rule_interest_lhs <- subset(groceryrules, lhs %ain% c('cooky/cakes', 'fermentedmilk'))
inspect(rule_interest_lhs[1:5])

#' Write groceryrules(write()) ------------------------------
write(harddrinks_rules, file = "./data/harddrinks_rules.csv", sep = ",", 
      quote = TRUE, row.names = FALSE)

#' Convert rules as datafame ------------------------------
harddrinks_rules_df <- as(harddrinks_rules, "data.frame")
str(harddrinks_rules_df)
head(harddrinks_rules_df)

harddrinks_rules_df[1:10, ] # %>% View()

#' Visualization arules :: arulesViz  ------------------------------
#' 
#' Plotting rules ------------------------------
groceryrules <- apriori(basket.transaction, parameter = list(support =0.0001, 
                                                             confidence = 0.0001,
                                                             minlen = 3))
plot(groceryrules)
plot(sort(groceryrules, by='support')[1:20], method='grouped')

# windows()
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

#' Before item ------------------------------
#' 
#' Before buying milk, what items would be purchased? ------------------------------
milk_before <- apriori(basket.transaction, 
                       parameter = list(support =0.0005, 
                                        confidence = 0.0001,
                                        minlen = 2), # eliminate white space...
                       appearance = list(default='lhs', rhs='milk'), # to know before items, put the item on rhs 
                       control = list(verbose=F))

inspect(milk_before[1:2, ])
inspect(sort(milk_before, by='confidence', decreasing = T)[1:5, ])

#' After item ------------------------------
#' 
#' After byying milk, what items would be purchased? ------------------------------
milk_after <- apriori(basket.transaction, 
                       parameter = list(support =0.0005, 
                                        confidence = 0.0001,
                                        minlen = 2), # eliminate white space...
                       appearance = list(default='rhs', lhs='milk'),
                       control = list(verbose=F))

inspect(sort(milk_after, by='confidence', decreasing = T)[1:10, ])


#' LoL Champoion Dataset :: sample-data.csv... ------------------------------
#' 
#' Load data and transder it into transactions format for apriori ------------------------------
df <- read_csv("data/sample-data-1.csv", locale = locale(encoding = "cp949"))

table(df$id) # 18 game players...
head(df)
dim(df)
length(unique(df$names))

rioter.list <- split(df$names, df$id)
rioter.transaction <- as(rioter.list, 'transactions')
rioter.transaction

#' Generate Rules ------------------------------
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

#' itemFrequencyPlot() ------------------------------
itemFrequencyPlot(rioter.transaction, topN=10)

#' image() ------------------------------
image(rioter.transaction[1:10])

#' Inspect rules ------------------------------
inspect(rules[1:5])

#' Arrangging with list ------------------------------
inspect(sort(rules, by = "lift")[1:5])

#' Find all rules which contains `자이라` ...------------------------------
zaira_rules <- subset(rules, items %in% "자이라")
inspect(zaira_rules)

#' Generate rules with condition list ------------------------------
rules <- apriori(rioter.transaction, parameter = list(supp=.06,
                                                      conf=.8))
summary(rules)

plot(sort(rules, by='support')[1:20], method='grouped')

# windows()
plot(rules, method='graph', control=list(type='items'),
     vertex.label.cex = .7, 
     edge.arrow.size= .3,
     edge.arrow.size= 2)


# 상품 방문 코너 예측...
# 고객당 과거 가장 많이 방문했던 코너들의 이름 순 구하기
# 한 행에 가장 많은 수에 해당하는 컬럼 명 구하기
# 가장 많은 수에 해당하는 코노명 5개 정리하기...
# 이것을 모델로 만들 수는 없을까? 각종 파생변수를 만들고 각장 최대로 방문한 매장을 목적변수로 하면 
# 그리고 그 모델이 예측한 각 분류값들의 확률을 순차적으로 반환하게 하면 되지 않을까?...
# pivotting to_long, to_wide에 대해 철저하게 연습하고 가로행 중 최대값을 가지는 열의 이름, 순차적으로 
# 5개의 열의 이름을 가져오는 방법을 열심히 연습해두자...
# 하루에 1번이상 커밋하고 꼭 문제를 일주에 한 문제정도는 연습하자
# 오늘 일을 잊지 말자.































