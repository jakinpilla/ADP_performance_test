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

#' 총 구매비용 구하기 :: g_paid----
head(tran)
tran %>% 
  group_by(custid) %>%
  summarise(g_paid = sum(amt)) -> cust_g_paid; head(cust_g_paid)

##----

## 총 구매 제품 종류의 수 구하기 :: buyed_prod_num----
## tran dataset에서는 하나의 row가 하나의 prod에 해당하므로 단순 count로 구한다.
head(tran)
tran %>%
  group_by(custid) %>%
  summarise(buyed_prod_num = n())  -> cust_buyed_prod_num; head(cust_buyed_prod_num)

##----

## 기간대별 구매금액의 비율 구하기 :: hf_xxx----

## 시간 변수를 생성--
as.character(tran$time) -> tran$time; glimpse(tran)
as.numeric(substr(tran$time, 1, 2)) -> tran$time; glimpse(tran)

## 시간 binning--
tran %>% mutate(h_bin = cut(time, 
                            breaks = c(0, 6, 12, 18, 23),
                            include.lowest = T, # 0을 그룹에 포함시키기 위해 반드시 필요, 아니면 NA값 반환됨.
                            labels=c('0-5', '6-11', '12-17', '18-23'))) -> tran; glimpse(tran)

## 고객별 시간대별 총 지출비용 구하기--
tran %>%
  group_by(custid, h_bin) %>%
  summarise(sum_paid_time_bin = sum(amt)) -> time_bin_paid; head(time_bin_paid)

## pivotting--
melted <- melt(time_bin_paid, id.vars=c('custid', 'h_bin'), 
               measure.vars = c('sum_paid_time_bin')); head(melted)
dcast(melted, custid ~ h_bin, value.var = 'value') -> dcasted; head(dcasted)
dcasted %>% replace(is.na(.), 0) -> cust_time_bin_paid; head(cust_time_bin_paid)

## hf_xxx 변수 만들기--
cust_time_bin_paid %>% mutate(total.amt = rowSums(.[-1])) -> cust_time_bin_sum; 
head(cust_time_bin_sum)
round(cust_time_bin_paid[, -1] / cust_time_bin_sum$total.amt, 2) -> cust_time_bin_ratio
head(cust_time_bin_ratio)

cust_time_bin_ratio <- cbind(custid =cust_time_bin_sum[, 1], cust_time_bin_ratio)
head(cust_time_bin_ratio)

paste0('hf', '_', colnames(cust_time_bin_paid[, -1]))
colnames(cust_time_bin_ratio) <- c('custid', paste0('hf', '_', colnames(cust_time_bin_paid[, -1])))
head(cust_time_bin_ratio)

## instead--
head(time_bin_paid)
time_bin_paid %>%
  spread(h_bin, sum_paid_time_bin) %>%
  replace(is.na(.), 0) %>% as.data.frame() %>%
  select(-custid) %>% 
  mutate(total.sum = rowSums(.)) -> df_tmp; head(df_tmp)

(as.matrix(df_tmp) / df_tmp$total.sum) %>%
  round(2) %>% as.data.frame() %>%
  select(-total.sum) %>% setNames(paste0('hf_', names(.))) %>% 
  cbind(custid = trans_grouped_df$custid, .) -> hf_df; head(hf_df)

##----

# 요일별 구매비용 비율 구하기 :: wdf_xxx ----
tran <- read.csv('./data/transaction.csv'); head(tran)

## 요일 변수 생성하기----
Sys.setlocale("LC_TIME", "English_United States.1252") # Windows
as.character(tran$ymd) -> tran$ymd; glimpse(tran)
as.Date(tran$ymd) -> tran$ymd; glimpse(tran)
tran$wd <- format(tran$ymd, '%a') ; glimpse(tran)
unique(tran$wd) # "Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"
tran$wd <- factor(tran$wd, levels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))
# 요일을 순서를 정의해 주는 것이 좋음
glimpse(tran)
Sys.setlocale("LC_TIME", "Korean_Korea.949") # Windows

## 고객별 요일별 구매비용의 합을 구하기
tran %>%
  group_by(custid, wd) %>%
  summarise(cust_wd_sum = sum(amt)) -> cust_wd_sum ; head(cust_wd_sum)

# pivotting----
melted <- melt(cust_wd_sum, id.vars = c('custid', 'wd'), measure.vars = c('cust_wd_sum')) 
head(melted)
dcast(melted, custid ~ wd, value.var = 'value') -> dcasted; head(dcasted)
dcasted %>% replace(is.na(.), 0) -> cust_wd_sum; head(cust_wd_sum)

# rowSum and ratio-----
cust_wd_sum %>% mutate(total.amt = rowSums(.[-1])) -> cust_wd_sum; head(cust_wd_sum)
round(cust_wd_sum[, -1] / cust_wd_sum$total.amt, 2) -> cust_wd_sum_ratio
head(cust_wd_sum_ratio)
cust_wd_sum_ratio %>% 
  select(-total.amt) -> cust_wd_sum_ratio; head(cust_wd_sum_ratio)

# var naming---
colnames(cust_wd_sum_ratio)
colnames(cust_wd_sum_ratio) <- paste0('wdf', '_', colnames(cust_wd_sum_ratio))
head(cust_wd_sum_ratio)

# cbind custid----
cust_wd_ratio <- cbind(custid =cust_wd_sum[, 1], cust_wd_sum_ratio)
head(cust_wd_ratio)

## instead--
head(cust_wd_sum)
cust_wd_sum %>%
  spread(wd, cust_wd_sum) %>%
  replace(is.na(.), 0) %>% as.data.frame() %>%
  select(-custid) %>%
  mutate(total.sum = rowSums(.)) -> wd_df_tmp; head(wd_df_tmp)

(as.matrix(wd_df_tmp)/wd_df_tmp$total.sum) %>%
  round(2) %>% as.data.frame() %>%
  select(-total.sum) %>% 
  setNames(paste0('wdf_', names(.))) %>% 
  cbind(custid = custid_df$custid, .) -> wdf_df; head(wdf_df)


# 구매한 서로 다른 제품의 수 :: purchased 변수 ----
head(tran)
melt(tran, id.vars = c('custid', 'prod'), measure.vars = c('amt')) -> melted
head(melted)

dcast(melted, custid ~ prod, value.var = 'value') -> dcasted; head(dcasted)
dcasted_mat <- as.matrix(dcasted)
ifelse(dcasted[, -1] > 0, 1, 0) -> dcasted_bin; dim(dcasted_bin)
rowSums(dcasted_bin)
rowSums(as.matrix(dcasted[, -1]))
data.frame(custid = dcasted$custid, 
           purchased = rowSums(as.matrix(dcasted[, -1]))) -> purchased_df; head(purchased_df)


# instead--
tran <- read.csv('./data/transaction.csv'); head(tran)

tran %>% 
  group_by(custid, prod) %>%
  summarise(sum.prod = sum(amt)) -> tran_grouped; head(tran_grouped); dim(tran_grouped)

tran_grouped %>%
  spread(prod, sum.prod) %>%
  replace(is.na(.), 0) -> tmp; head(tmp)

as.matrix(tmp[, -1]) -> tmp_mat
ifelse(tmp_mat > 0, 1, 0) -> tmp_mat_bin

as.data.frame(tmp_mat_bin) %>%
  mutate(total.sum = rowSums(.)) %>%
  select(total.sum) -> purchase_df; head(purchase_df); class(purchase_df)

custid_df %>% head

data.frame(custid = custid_df$custid, purchase = purchase_df$total.sum) -> purchase_df; head(purchase_df)

# 제품 카테고별 구매비용 변동계수(카테고리별 구매비용의 표준편차 / 평균) :: ct_cov 변수----
head(tran)
prod_ct_tbl <- read.csv('./data/prod_cate.csv'); head(prod_ct_tbl)
colnames(prod_ct_tbl) <- c('prod', 'prod_ct') ; head(prod_ct_tbl)
length(unique(prod_ct_tbl$prod_ct)) ## 84 --> 11

## join----
as.tibble(tran) -> tran
tran %>% left_join(prod_ct_tbl, by='prod') -> tran
head(tran)

tran %>%
  group_by(custid, prod_ct) %>%
  summarise(sum_amt = sum(amt)) %>%
  spread(prod_ct, sum_amt) %>%
  replace(is.na(.), 0) -> tran_tmp

head(tran_tmp)

tran_tmp %>%
  ungroup() %>%
  select(-custid) -> tran_tmp; head(tran_tmp)

library(matrixStats)
tran_ct_mat <- as.matrix(tran_tmp)
tran_tmp$ct_mean <- rowMeans(tran_tmp); head(tran_tmp)
View(head(tran_tmp))

tran_tmp$std <- rowSds(tran_ct_mat) ; head(tran_tmp)
View(head(tran_tmp))

## ct_cov 변수 생성 및 custid 붙이기----
tran_tmp %>% 
  select(ct_mean, std) %>%
  mutate(ct_cov = std/ct_mean) %>% as.data.frame() %>%
  cbind(custid = custid_df$custid, .) %>%
  select(custid, ct_cov) -> ct_cov_df; head(ct_cov_df)

## 총 구매일수 :: vdays 변수 ----
## 지금부터는 day를 기준으로...
head(tran)
tran %>%
  group_by(custid) %>%
  summarise(vdays = n_distinct(ymd)) -> vday_df; head(vday_df)
head(vday_df)

## 하루 평균 구매 비용 :: day_mean_amt 변수 ----
cust_g_paid %>% 
  left_join(vday_df, by='custid') %>%
  mutate(day_mean_amt = (g_paid/vdays)) %>%
  select(custid, day_mean_amt) -> day_mean_amt_df; head(day_mean_amt_df)

## 일별 변동계수 :: 일일 구매비용의 `표준편차/평균` 값 :: daycov 변수 ----
tran %>%
  group_by(custid, ymd) %>%
  summarise(sum_day_amt = sum(amt)) %>% as.data.frame() %>%
  group_by(custid) %>%
  summarise(day_sd_amt = sd(sum_day_amt)) %>%
  replace(is.na(.), 0) -> day_sd_amt_df; 

head(day_sd_amt_df)

day_sd_amt_df %>%
  left_join(day_mean_amt_df, by='custid') %>%
  mutate(daycov = (day_sd_amt / day_mean_amt)) %>%
  select(custid, daycov) -> daycov_df 

head(daycov_df)

## 가장 많이 구매한 제품 :: top_prod 변수 :: dplyr :: first() ----
head(tran)

tran %>%
  select(custid, prod) %>%
  group_by(custid, prod) %>%
  summarise(count = n()) %>%
  arrange(custid, desc(count)) %>%
  group_by(custid) %>%
  summarise(top_prod = first(prod)) -> top_prod_df; head(top_prod_df)


## ','로 고객별 구매한 물품을 하나이 row로 표현하기
tran <- read.csv('./data/transaction.csv'); head(tran)
tran %>%
  select(custid, prod) -> custid_prod_df

custid_prod_df %>%
  group_by(custid) %>%
  mutate(item_pur = paste(prod, collapse = ',')) -> item_pur_df; head(item_pur_df)

tran %>% 
  filter(custid == 'C2048') # softdrink를 많이 구매하는 사람인지 여부 확인

## 반대로는 어떻게 할 수 있을까??


# cbinding every data----
# 1. cust_prod_ct_ratio
# 2. cust_coverage
# 3. cust_g_paid
# 4. cust_buyed_prod_num
# 5. cust_time_bin_ratio
# 6. cust_wd_ratio
# 7. purchased_df
# 8. ct_cov_df
# 9. vday_df
# 10. day_mean_amt_df
# 11. daycov_df 
# 12. top_prod_df

head(cust_prod_ct_ratio)

cust_prod_ct_ratio %>%
  left_join(cust_coverage, by='custid') %>%
  left_join(cust_g_paid, by='custid') %>%
  left_join(cust_buyed_prod_num) %>%
  left_join(cust_time_bin_ratio) %>%
  left_join(cust_wd_ratio) %>%
  left_join(purchase_df) %>%
  left_join(ct_cov_df) %>%
  left_join(vday_df) %>%
  left_join(day_mean_amt_df) %>%
  left_join(daycov_df) %>%
  left_join(top_prod_df) -> data_total

# 나중에 custid는 모두 factor로 변환하자.(warning)
fix(data_total)
write.csv(data_total, './data/data_total.csv') 

# with click streaming data----
## click-streaming data에서 시간은 transaction 데이터에서의 구매비용이라고 생각하면 된다.
## click-streaming data에서 사이트는 transaction 데이터에서의 제품이라고 생각하면 된다.

# CT_xxx :: 웹사이트 카테고리 별 체류시간 비율(제품 카테고리별 구매비용 비율)
# COVERAGE :: 서로 다른 웹 사이트에 얼마나 다양하게 접속했는지에 대한 비율
#             (서로 다른 카테고리 제품을 얼마나 다양하게 구매하였는지에 대한 비율, 구매 제품 카테고리수 / 전체 카테고리 수)
# G_DWELLTIME :: 총 체류시간(총 구매비용)
# PAGEVIEWS :: 총 페이지뷰(총 구매 제품의 갯수)
# HF_xxx :: 시간대별(0-5시, 6-11시, 12-17시, 18-23시) 체류시간 비율(시간대별 구매금액 비율)
# DF_xxx :: 요일별 체류시간 비율(-> 요일별 구매비용의 비율)
# VISITES :: 접속한 서로 다른 웹사이트의 수(구매한 서로 다른 제품의 수)
# SITECOV :: 웹사이트 카케고리 별 체류시간 변동계수(카테고리별 체류시간의 '표준편차/평균' 값) 
#            (제품 카테고별 구매비용 변동계수(카테고리별 구매비용의 표준편차 / 평균))
# VDAYS :: 총 접속일수(총 매장 방문일수)
# DAYTIME :: 하루 평균 체류시간(하루 평균 구매비용)
# DAYCOV :: 일별 변동계수(일일 체류시간의 '표준편차/평균' 값)
#           (일일 구매비용의 '표준편차/평균' 값)
# SCH_KEYWORDS :: 네이버에서 검색한 검색량(transaxtion data에 matching 하기가 곤란함 --> 어떻게 전처리??)
# SCH_TOPKEYWORD :: 네이버에서 가장 많이 검색한 검색어(가장 많이 구매한 상품 --> word embedding 필요)
# GENDER :: 고객성별(남자/여자). 예측하고자 하는 값

