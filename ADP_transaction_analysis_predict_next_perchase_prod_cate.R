#' ---
#' title: "ADP transaction data analysis predict next perchase prod cate"
#' author: "jakinpilla"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::github_document
#' ---

setwd("C:/Users/Daniel/ADP_performance_test")
getwd()

#+ setup, message=FALSE
Packages <- c('tidyverse', 'data.table', 'reshape2', 'caret', 'rpart', 'GGally', 
              'ROCR', 'party', 'randomForest', 'knitr')
lapply(Packages, library, character.only=T)


#' 아래와 같이 거래데이터를 불러오고 그 데이터와 prod_cate 데이터를 조인한다.
tran <- read_csv("data/transaction.csv"); head(tran)
nrow(tran)

tran %>% colnames()

prod_ct_tbl <- read.csv('./data/prod_cate.csv', stringsAsFactors = F)
colnames(prod_ct_tbl) <- c('prod', 'prod_ct') ; head(prod_ct_tbl)

length(unique(prod_ct_tbl$prod_ct)) ## 84 --> 11

as_tibble(tran) -> tran
tran %>% left_join(prod_ct_tbl, by='prod') -> tran
tran

# cust의 수
tran %>% select(custid) %>% unique() # 2089ㅁ

tran %>% 
  group_by(custid, prod_ct) %>%
  summarise(sum.prod_ct = sum(amt)) -> tran_grouped; head(tran_grouped); dim(tran_grouped)

tran %>% colnames()


# spread...
str(tran_grouped)


# 고객별 prod_ct별 소비 금액, wide form 만들고 가장 많이 소비된 항목을 표시하기...
tran_grouped %>%
  slice(which.max(sum.prod_ct))

tran_grouped %>%
  spread(prod_ct, sum.prod_ct) %>%
  left_join(
    tran_grouped %>%
      slice(which.max(sum.prod_ct)) %>%
      select(custid, prod_ct) %>%
      rename(first_prod_ct = prod_ct), by = 'custid'
  ) 
  

# 고객별 prod_ct별 가장 많이 소비된 항목과 두 번째로 많이 소비된 항목 표시하기...

tran_grouped

tran_grouped %>%
  arrange(desc(sum.prod_ct), .by_group = T) %>%
  slice(2) %>% 
  select(custid, prod_ct) %>%
  rename(second_prod_ct = prod_ct)


tran_grouped %>%
  spread(prod_ct, sum.prod_ct) %>%
  left_join(
    tran_grouped %>%
      slice(which.max(sum.prod_ct)) %>%
      select(custid, prod_ct) %>%
      rename(first_prod_ct = prod_ct), by = 'custid') %>% 
  left_join(
    tran_grouped %>%
      arrange(desc(sum.prod_ct), .by_group= T) %>%
      slice(2) %>% 
      select(custid, prod_ct) %>%
      rename(second_prod_ct = prod_ct), by= 'custid') -> df_2nd

View(df_2nd)
