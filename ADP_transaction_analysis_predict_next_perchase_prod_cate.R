#' ---
#' title: "ADP transaction data analysis predict next perchase prod cate"
#' author: "jakinpilla"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::github_document
#' ---

setwd("C:/Users/Daniel/ADP_performance_test")
getwd()

#' 거래 데이터 분석
#' 거래 데이터 분석을 위한 고객별 파생변수를 어떻게 생성하는지를 알아온다.
#' 이번 장의 목적은 일종의 고객 데이터 feature engineering이라고 할 수 있다.
#' 
#' 일단 분석에 필요한 여러 개의 패키지를 한 번에 읽는다.

#+ setup, message=FALSE
Packages <- c('tidyverse', 'data.table', 'reshape2', 'caret', 'rpart', 'GGally', 
              'ROCR', 'party', 'randomForest', 'knitr')
lapply(Packages, library, character.only=T)


#' 일단 거래데이터를 아래와 같이 불러온다.
tran <- read_csv("data/transaction.csv"); head(tran)

#' ## 제품 카테고리별 구매비용 비율 :: ct_xxx 파생변수 만들기
#' 
#'고객이 각 물품 카테고리별로 지출한 총금액의 비율을 알아보자.
length(unique(tran$prod))

#' instead...
tran %>%
  select(prod) %>%
  summarise(dict_count =n_distinct(prod))

#' 물품종류의 총 갯수는 84개이다.
#'
#' like python value.count() :: 각 물품별 거래데이터에서 등장횟수는 다음과 같이 손쉽게 구할 수 있다.

tran %>%
  select(prod) %>%
  group_by(prod) %>%
  tally() %>% arrange(desc(n))

#' 위의 실행결과에서 알 수 있듯이 softdrink 가 가장 많이 거래되는 물품이다.
#' 
#' ### Load prod_cate.csv and join the tbl
#' 
#' 각 물품이 어느 카테고리에 해당하는가에 대한 테이블을 불러온다.
prod_ct_tbl <- read.csv('./data/prod_cate.csv', stringsAsFactors = F)
colnames(prod_ct_tbl) <- c('prod', 'prod_ct') ; head(prod_ct_tbl)

length(unique(prod_ct_tbl$prod_ct)) ## 84 --> 11

#' 거래데이터에서 각 물품들은 11개의 카테고리로 분류된다.
#' 
#' joining ::  위의 테이블을 원래 거래 데이터와 조인한다.
#' 
as_tibble(tran) -> tran
tran %>% left_join(prod_ct_tbl, by='prod') -> tran
tran
#' 위와 같이 조인된 결과를 확인할 수 있다.
#' 
#' 우리는 고객별, 카테고리별 총 지출금의 비율을 알아보는 것이므로 custid와 prod_ct를 group_by하고 각 그룸별 지출 총액을 다음과 같이 구한다.
tran %>% 
  group_by(custid, prod_ct) %>%
  summarise(sum.prod_ct = sum(amt)) -> tran_grouped; head(tran_grouped); dim(tran_grouped)

#' ### Pivotting
#' 
#' Pivotting을 통해 일단 각 고객별, 카테고리별 총 지출금액에 대한 데이터를 확보한다.
molten <- melt(tran_grouped, id.vars=c('custid', 'prod_ct'), measure.vars = c('sum.prod_ct'))
dcasted <- dcast(molten, custid ~ prod_ct, value.var = 'value') -> dcasted
dcasted %>% replace(is.na(.), 0) -> cust_prod_ct_amt; 
head(cust_prod_ct_amt) %>% kable()

cust_prod_ct_amt %>% View()
