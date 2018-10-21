# setting working dtrectory----
setwd("C:/Users/Daniel/ADP_performance_test")
getwd()

# install.packages('yardstick')
# install.packages('party')
# install.packages('randomForest')

# 여러 개의 패키지를 한 번에 읽기
Packages <- c('tidyverse', 'data.table', 'reshape2', 'caret', 'rpart', 'GGally', 'ROCR', 'party', 
              'randomForest')
lapply(Packages, library, character.only=T)

# feature engineering----

## with transaction data----
tran <- read.csv('./data/transaction.csv'); head(tran)

## 제품 카테고리별 구매비용 비율 :: ct_xxx 파생변수 만들기 ----
length(unique(tran$prod))

prod_ct_tbl <- read.csv('./data/prod_cate.csv'); head(prod_ct_tbl)
colnames(prod_ct_tbl) <- c('prod', 'prod_ct') ; head(prod_ct_tbl)
length(unique(prod_ct_tbl$prod_ct)) ## 84 --> 11

## join----
as.tibble(tran) -> tran
tran %>% left_join(prod_ct_tbl, by='prod') -> tran
tran %>% 
  group_by(custid, prod_ct) %>%
  summarise(sum.prod_ct = sum(amt)) -> tran_grouped

# pivotting----
melted <- melt(tran_grouped, id.vars=c('custid', 'prod_ct'), measure.vars = c('sum.prod_ct')); head(melted)
dcast(melted, custid ~ prod_ct, value.var = 'value') -> dcasted; head(dcasted)
dcasted %>% replace(is.na(.), 0) -> cust_prod_ct_amt; head(cust_prod_ct_amt)

# rowsum and ratio----
cust_prod_ct_amt %>% mutate(total.amt = rowSums(.[-1])) -> cust_prod_ct_sum; head(cust_prod_ct_sum)
round(cust_prod_ct_amt[, -1] / cust_prod_ct_sum$total.amt, 2) -> cust_prod_ct_ratio
class(cust_prod_ct_ratio)
cust_prod_ct_ratio <- cbind(custid =cust_prod_ct_amt[, 1], cust_prod_ct_ratio); head(cust_prod_ct_ratio)
paste0('ct', '_', colnames(cust_prod_ct_ratio[, -1]))
colnames(cust_prod_ct_ratio) <- c('custid', paste0('ct', '_', colnames(cust_prod_ct_ratio[, -1])))
head(cust_prod_ct_ratio)

## 서로 다른 카테고리의 제품을 얼마나 다양하게 구매하였는지에 대한 비율 :: coverage 파생변수 만들기----
head(cust_prod_ct_amt)
## 0이 아닌 컬럼의 수 구하기
cust_prod_ct_mat <- as.matrix(cust_prod_ct_amt[, -1])
cust_prod_ct_mat_bin <- ifelse(cust_prod_ct_mat[, ] > 0, 1, 0) # 구매했으면 1, 안 했으면 0으로 치환
round(rowSums(cust_prod_ct_mat_bin) / 11, 2)
cust_prod_ct_amt$coverage <- round(rowSums(cust_prod_ct_mat_bin) / 11, 2)
cust_coverage <- cust_prod_ct_amt[, c('custid', 'coverage')]; head(cust_coverage)

## 총 구매비용 구하기 :: g_paid----
head(tran)
tran %>% 
  group_by(custid) %>%
  summarise(g_paid = sum(amt)) -> cust_g_paid; head(cust_g_paid)

## 총 구매 제품 종류의 수 구하기 :: buyed_prod_num----
## tran dataset에서는 하나의 row가 하나의 prod에 해당하므로 단순 count로 구한다.
head(tran)
tran %>%
  group_by(custid) %>%
  summarise(buyed_prod_num = n())  -> cust_buyed_prod_num; head(cust_buyed_prod_num)


## 기간대별 구매금액의 비율 구하기 :: hf_xxx----

## 시간 변수를 생성----
as.character(tran$time) -> tran$time; glimpse(tran)
as.numeric(substr(tran$time, 1, 2)) -> tran$time; glimpse(tran)

## 시간 binning----
tran %>% mutate(h_bin = cut(time, 
                            breaks = c(0, 6, 12, 18, 23),
                            include.lowest = T, # 0을 그룹에 포함시키기 위해 반드시 필요, 아니면 NA값 반환됨.
                            labels=c('0-5', '6-11', '12-17', '18-23'))) -> tran; glimpse(tran)

## 고객별 시간대별 총 지출비용 구하기----
tran %>%
  group_by(custid, h_bin) %>%
  summarise(sum_paid_time_bin = sum(amt)) -> time_bin_paid; head(time_bin_paid)

## pivotting----
melted <- melt(time_bin_paid, id.vars=c('custid', 'h_bin'), 
               measure.vars = c('sum_paid_time_bin')); head(melted)
dcast(melted, custid ~ h_bin, value.var = 'value') -> dcasted; head(dcasted)
dcasted %>% replace(is.na(.), 0) -> cust_time_bin_paid; head(cust_time_bin_paid)

## hf_xxx 변수 만들기----
cust_time_bin_paid %>% mutate(total.amt = rowSums(.[-1])) -> cust_time_bin_sum; 
head(cust_time_bin_sum)
round(cust_time_bin_paid[, -1] / cust_time_bin_sum$total.amt, 2) -> cust_time_bin_ratio
head(cust_time_bin_ratio)

cust_time_bin_ratio <- cbind(custid =cust_time_bin_sum[, 1], cust_time_bin_ratio)
head(cust_time_bin_ratio)

paste0('hf', '_', colnames(cust_time_bin_paid[, -1]))
colnames(cust_time_bin_ratio) <- c('custid', paste0('hf', '_', colnames(cust_time_bin_paid[, -1])))
head(cust_time_bin_ratio)

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

## custid 붙이기----
cust_g_paid %>%
  head

tran_tmp %>% 
  select(ct_mean, std) %>%
  mutate(ct_cov = std/ct_mean) %>% 
  cbind(cust_g_paid[, 1]) %>% 
  as.tibble() %>% 
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

## 가장 많이 구매한 제품 :: top_prod 변수 ----
head(tran)

tran %>%
  select(custid, prod) %>%
  group_by(custid, prod) %>%
  summarise(count = n()) %>%
  arrange(custid, desc(count)) %>%
  group_by(custid) %>%
  summarise(top_prod = first(prod)) -> top_prod_df


## ','로 고객별 구매한 물품을 하나이 row로 표현하기
custid_prod_df %>%
  group_by(custid) %>%
  mutate(item_pur = paste(prod, collapse = ',')) -> item_pur_df; head(item_pur_df)

head(item_pur_df)

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
  left_join(purchased_df) %>%
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

