#' ---
#' title: "ADP R Basic(transaction data analysis)"
#' author: "jakinpilla"
#' date: "May 3rd, 2014"
#' output: rmarkdown::github_document
#' ---

#' setting working dtrectory
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

#' ### Rowsum and Ratio
#' 
#' 이제는 각 카테고리별 지출 금액의 비율을 알아보기 위해 위의 데이터를 각각의 고객이 지출한 지출금액으로 나눈다.
#' 
#' 이를 위해 고객별 총 지출금액은 다음과 같이 구할 수 있다.
cust_prod_ct_amt %>%
  mutate(total.amt = rowSums(.[-1])) -> cust_prod_ct_sum; head(cust_prod_ct_sum)

#' 이제 총 지출금액으로 데이터를 나누어 `ratio`를 구한다. ratio를 구할때 소수점 아랫자리 2째 자리까지 계산하여 사용한다. 계산을 위해 가장 첫 번째 컬럼인 custid는 제외시킨다.(`cust_prod_ct_amt[, -1]`)
round(cust_prod_ct_amt[, -1] / cust_prod_ct_sum$total.amt, 2) -> cust_prod_ct_ratio
cust_prod_ct_ratio[1:10, 1:10]

#' 다시 고객 아이디를 결합하여 데이터를 만들고, 생성된 데이터의 변수의 이름을 `ct_xx` 형식으로 만들어 데이터를 완성한다.
cust_prod_ct_ratio <- cbind(custid = cust_prod_ct_amt[, 1], cust_prod_ct_ratio)
colnames(cust_prod_ct_ratio) <- c('custid', paste0('ct', '_', colnames(cust_prod_ct_ratio[, -1])))
head(cust_prod_ct_ratio) %>% kable()

#' ### Pivotting and calculating ratio with dplyr more easily...
#' 이번에는 pivottig을 보다 쉽게하는 다른 방법에 대해 알아보자.
head(tran_grouped)

#' 위의 데이터에서 `dplyr`의 `spread()` 함수를 이용하면 상당히 편리하게 데이터 변형이 가능하다. 시험장에서는 가급적 이 방법으로 데이터를 전처리한다.
tran_grouped %>%
  spread(prod_ct, value = sum.prod_ct) %>%
  replace(is.na(.), 0) %>% as.data.frame() -> trans_grouped_df
head(trans_grouped_df)

#' 추후 고객 아이디를 조인하는 연산이 많이 등장하므로 고객아이디만을 컬럼으로 가지는 data.frame()를 정의해두자.
data.frame(custid = trans_grouped_df$custid) -> custid_df; head(custid_df)

#' 고객 아이디를 제외하여 두고 각 고객이 지출한 금액의 총합을 구해 마지막 열에 생성하는 코드를 다음과 같이 작성할 수 있다. 
trans_grouped_df %>%  
  select(-custid) %>%
  mutate(total.sum = rowSums(.)) -> tran_g_spread; head(tran_g_spread)

#' 위의 데이터 프레임 형식의 데이터를 `matrix` 형식으로 변환하고 이를 총합으로 나누어 소수점 2짜자리까지 남겨둔 다음 다시 데이터 프레임 형식으로 변환한다. 그리고 마지막 열인 `total.sum`` 컬럼은 제거한다.
(as.matrix(tran_g_spread) / tran_g_spread$total.sum) %>%
  round(2) %>% as.data.frame() %>%
  select(-total.sum) -> prod_ct_ratio; head(prod_ct_ratio)

#' 위에서 생성된 데이터의 컬럼이름을 `ct_xx` 형식으로 변환한다.
colnames(prod_ct_ratio) <- paste0('ct_', colnames(prod_ct_ratio)); head(prod_ct_ratio)

#' 마지막으로 고객아이디 데이터프레임과 결합하여 최종 데이터를 완성한다.
ct_ratio_df <- cbind(custid = custid_df$custid, 
                     prod_ct_ratio)

head(ct_ratio_df) %>% kable()

#' ## Coverage 파생변수
#' 서로 다른 카테고리의 제품을 얼마나 다양하게 구매하였는지에 대한 비율을 구한다.
#' 한 고객이 물건을 다양하게 구매하였는지 아니면 몇 안 되는 물품만을 구매하였는지에 대한 변수이다. 흔히 여성 고객의 경우 물품을 다양하게 구매하는 반면 남성 고객은 물품을 편협하게 구매하는 경향이 있다.
#' 

head(trans_grouped_df)

#' 일단 위의 데이터에서 0이 아닌 컬럼의 수를 구한다. 이를 구하기 위해 데이터를 matrix 형식으로 변환 후 cust_id를 제거하고 구매를 했으면 1 하지 않았으면 0인 데이터를 만들어본다.

trans_grouped_mat <- as.matrix(trans_grouped_df[, -1])
trans_grouped_mat_bin <- ifelse(trans_grouped_mat[,] > 0, 1, 0) # 구매했으면 1, 안 했으면 0

#' 위의 데이터를 가로방향으로 더한 후 합계를 전체 카테고리 수로 나누면 각각의 고객이 얼마나 다양한 제품을 구매하였는지를 알 수 있는 데이터를 `vector` 형식으로 구할 수 있다.
round(rowSums(trans_grouped_mat_bin) / 11, 2) -> vec.coverage  # 11개의 카테고리이므로 11로 나눔
vec.coverage[1:10]; length(vec.coverage)

#' 이렇게 구한 `vector` 데이터를 `custid_df`에 `mutate()` 한다.
custid_df %>%
  mutate(coverage = vec.coverage) -> df.coverage
head(df.coverage)

#' 위의 과정을 보다 편리하게 할 수 있는 방법은 없을까?
ncol.num <- ncol(trans_grouped_df)-1 # 먼저 추후 나누게될 카테고리의 수를 구해 저장해 놓는다.

trans_grouped_df %>%
  select(-custid) %>%
  mutate_all(~ replace(., . > 0, 1)) %>% # mutate_all(~replace(., "condition", value)) 형식을 이용하면 
  # 특정조건을 만족하는 데이터프레임의 전 원소를 특정한 값으로 한 번에 변환할 수 있다.
  mutate(total.sum = rowSums(.)) %>%
  mutate(coverage = total.sum/ ncol.num) %>% # 전체 카테고리수로 total.sum을 나눈다.
  mutate(coverage = round(coverage, 2)) %>%
  select(coverage) -> coverage_df; head(coverage_df)

cbind(custid_df, coverage_df) -> coverage_df; head(coverage_df)



