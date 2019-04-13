setwd("C:/Users/Daniel/ADP_performance_test")
getwd()

Packages <- c('tidyverse', 'data.table', 'reshape2', 'caret', 'rpart', 'GGally', 
              'ROCR', 'party', 'randomForest')
lapply(Packages, library, character.only=T)

# feature engineering
tran <- read_csv('./data/transaction.csv'); head(tran)

# ct_xxx 변수 : 제품 카테고리별 구매비용 비율 ----
length(unique(tran$prod))

## instead..
tran %>%
  select(prod) %>%
  summarise(dict_count =n_distinct(prod))

## like python value.count()--
tran %>%
  select(prod) %>%
  group_by(prod) %>%
  tally() %>% arrange(desc(n))

## load prod_cate.csv and join the tbl--
prod_ct_tbl <- read_csv('./data/prod_cate.csv'); head(prod_ct_tbl)
colnames(prod_ct_tbl) <- c('prod', 'prod_ct') ; head(prod_ct_tbl)
length(unique(prod_ct_tbl$prod_ct)) ## 84 --> 11

## join between transaction data and product category table...
as.tibble(tran) -> tran
tran %>% left_join(prod_ct_tbl, by='prod') -> tran
tran %>% 
  group_by(custid, prod_ct) %>%
  summarise(sum.prod_ct = sum(amt)) -> tran_grouped; head(tran_grouped); dim(tran_grouped)

# pivotting with melt and dcast...
melted <- melt(tran_grouped, id.vars=c('custid', 'prod_ct'), measure.vars = c('sum.prod_ct')); 
dcast(melted, custid ~ prod_ct, value.var = 'value') -> dcasted; head(dcasted)
dcasted %>% replace(is.na(.), 0) -> cust_prod_ct_amt; head(cust_prod_ct_amt)

# rowsum and ratio----
cust_prod_ct_amt %>% mutate(total.amt = rowSums(.[-1])) -> cust_prod_ct_sum; head(cust_prod_ct_sum)
round(cust_prod_ct_amt[, -1] / cust_prod_ct_sum$total.amt, 2) -> cust_prod_ct_ratio
cust_prod_ct_ratio <- cbind(custid =cust_prod_ct_amt[, 1], cust_prod_ct_ratio); head(cust_prod_ct_ratio)
paste0('ct', '_', colnames(cust_prod_ct_ratio[, -1]))
colnames(cust_prod_ct_ratio) <- c('custid', paste0('ct', '_', colnames(cust_prod_ct_ratio[, -1])))
head(cust_prod_ct_ratio)

# pivotting with simple spread...
tran_grouped %>%
  spread(prod_ct, sum.prod_ct, fill = 0) -> cust_prod_ct_amt; cust_prod_ct_amt

# pivotting with more robust spread...
tran_grouped %>%
  rowid_to_column(var = "id") %>%
  spread(prod_ct, sum.prod_ct, fill = 0) %>%
  select(-id) %>%
  group_by(custid) %>%
  summarise_at(vars(-custid), sum) -> cust_prod_ct_amt; cust_prod_ct_amt

# robust spread function definition-----
id_spread_sum <- function(df.grouped) {
  df.grouped %>%
    rowid_to_column(var = "id") %>%
    spread(eval(colnames(df.grouped[, 2])), eval(colnames(df.grouped[, 3])), fill = 0) %>%
    select(-id) %>%
    group_by(custid) %>%
    summarise_at(vars(-custid), sum) -> df.result
  
  return(df.result)
}

id_spread_sum(tran_grouped) -> cust_prod_ct_amt; cust_prod_ct_amt

# instead rowsum and ratio with dplyr----
#  rowSums, mutate_at, select_if, rename_at...
tran_grouped %>%
  id_spread_sum() %>%
  mutate(total = rowSums(select_if(., is.numeric))) %>%
  mutate_at(vars(-custid), funs(round(./total, 2))) %>%
  rename_at(vars(-custid), ~ paste0("ct_", .))

tran_grouped %>%
  id_spread_sum() %>%
  mutate(total = rowSums(select_if(., is.numeric))) %>%
  mutate_at(vars(-custid), funs(round(./total, 2))) %>%
  rename_at(vars(-custid), ~ paste0("ct_", .)) %>% select(-ncol(.)) 

# functionize...
rowsum_ratio_df <- function(grouped_df, prefix) {
  grouped_df %>%
    id_spread_sum() %>%
    mutate(total = rowSums(select_if(., is.numeric))) %>%
    mutate_at(vars(-custid), funs(round(./total, 2))) %>%
    rename_at(vars(-custid), ~ paste0(prefix, .)) %>% select(-ncol(.)) -> df.result
  
  return(df.result)
}

tran_grouped %>% rowsum_ratio_df(., "ct_") -> cust_prod_ct_ratio

# coverage 변수 ----
id_spread_sum(tran_grouped) -> cust_prod_ct_amt; cust_prod_ct_amt
mat.cust_prod_ct_amt <- as.matrix(cust_prod_ct_amt[, -1])
mat_bin.cust_prod_ct_amt <- ifelse(mat.cust_prod_ct_amt[, ] > 0, 1, 0) # 구매했으면 1, 안 했으면 0으로 치환
round(rowSums(mat_bin.cust_prod_ct_amt) / 11, 2) -> tmp; length(tmp) # vector로 rowSum value들을저장

cust_prod_ct_amt$coverage <- tmp
head(cust_prod_ct_amt)
cust_coverage <- cust_prod_ct_amt[, c('custid', 'coverage')]; head(cust_coverage)

## instead...
id_spread_sum(tran_grouped) -> cust_prod_ct_amt; cust_prod_ct_amt
as.matrix(cust_prod_ct_amt[, -1]) -> ct_ratio_mat 
dim(ct_ratio_mat)
ct_ratio_mat[1:10, 1:11] # matrix 변환 전 custid 컬럼을 제거해야 함에 유의
ifelse(ct_ratio_mat > 0, 1, 0) -> ct_bin_mat; ct_bin_mat[1:10, 1:11]
ncol(ct_bin_mat)
dim(ct_bin_mat)

as.data.frame(ct_bin_mat) %>% 
  mutate(total.sum = rowSums(.)) %>%
  mutate(coverage = total.sum/ncol(ct_bin_mat)) %>%
  mutate(coverage = round(coverage, 2)) %>%
  select(coverage) -> coverage_df; head(coverage_df)

cbind(custid = cust_prod_ct_amt$custid, coverage_df) -> coverage_df ; head(coverage_df)

# instead...
tran_grouped %>%
  id_spread_sum() %>%
  mutate_at(vars(-custid), funs(ifelse(. > 0, 1, 0))) %>%
  mutate(total = rowSums(select_if(., is.numeric))) %>% 
  mutate(coverage = (total / (ncol(.) - 2))) %>% # custid, total 2개의 변수 제외
  mutate(coverage = round(coverage, 2)) %>%
  select(custid, coverage) -> cust_coverage

# g_parid : 총 구매비용 구하기----
head(tran)
tran %>% 
  group_by(custid) %>%
  summarise(g_paid = sum(amt)) -> cust_g_paid; head(cust_g_paid)

# buyed_prod_num : 총 구매 제품 종류의 수 구하기----
## tran dataset에서는 하나의 row가 하나의 prod에 해당하므로 단순 count로 구한다.
head(tran)
tran %>%
  group_by(custid) %>%
  summarise(buyed_prod_num = n())  -> cust_buyed_prod_num; head(cust_buyed_prod_num)

# hf_xxx : 기간대별 구매금액의 비율 구하기----
## 시간 변수 생성...
tran <- read_csv('./data/transaction.csv')
tran$time <- as.character(tran$time); glimpse(tran)
as.numeric(substr(tran$time, 1, 2)) -> tran$time; glimpse(tran)

tran <- read_csv('./data/transaction.csv')
tran %>%
  mutate(time = as.character(time)) %>%
  mutate(time = substr(time, 1, 2)) %>%
  mutate(time = as.numeric(time)) -> tran

tran <- read_csv('./data/transaction.csv')
tran$time <- tran$time %>% as.character() %>% substr(., 1, 2) %>% as.numeric
tran

## 시간 binning...
tran %>% mutate(h_bin = cut(time, 
                            breaks = c(0, 6, 12, 18, 23),
                            include.lowest = T, # 0을 그룹에 포함 위해 필요, 아니면 NA값 반환
                            labels=c('0-5', '6-11', '12-17', '18-23'))) -> tran; tran

## 고객별 시간대별 총 지출비용 구하기...
tran %>%
  group_by(custid, h_bin) %>%
  summarise(sum_paid_time_bin = sum(amt)) -> time_bin_paid; head(time_bin_paid)

## pivotting--
melted <- melt(time_bin_paid, id.vars=c('custid', 'h_bin'), 
               measure.vars = c('sum_paid_time_bin')); head(melted)
dcast(melted, custid ~ h_bin, value.var = 'value') -> dcasted; head(dcasted)
dcasted %>% replace(is.na(.), 0) -> cust_time_bin_paid; head(cust_time_bin_paid)

# instead...
id_spread_sum(time_bin_paid)

## hf_xxx 변수 만들기--

## 1)...
cust_time_bin_paid %>% mutate(total.amt = rowSums(.[-1])) -> cust_time_bin_sum; 
head(cust_time_bin_sum)
round(cust_time_bin_paid[, -1] / cust_time_bin_sum$total.amt, 2) -> cust_time_bin_ratio
head(cust_time_bin_ratio)

cust_time_bin_ratio <- cbind(custid =cust_time_bin_sum[, 1], cust_time_bin_ratio)
head(cust_time_bin_ratio)

paste0('hf', '_', colnames(cust_time_bin_paid[, -1]))
colnames(cust_time_bin_ratio) <- c('custid', paste0('hf', '_', colnames(cust_time_bin_paid[, -1])))
head(cust_time_bin_ratio)

## 2)...
head(time_bin_paid)
time_bin_paid %>%
  ungroup() %>%
  rowid_to_column() %>%
  spread(h_bin, sum_paid_time_bin, fill = 0) %>%
  select(-rowid) %>%
  group_by(custid) %>%
  summarise_at(vars(-custid), sum) %>%
  mutate(row_sum = rowSums(select_if(., is.numeric))) %>%
  mutate_at(vars(-custid), funs(round(./row_sum, 2))) %>%
  select(-row_sum) 

## 3)...
time_bin_paid %>% rowsum_ratio_df(., "hf_") -> hf_df; hf_df

# wdf_xxx :요일별 구매비용 비율 구하기----
tran <- read_csv('./data/transaction.csv'); head(tran)

## 요일 변수 생성...
Sys.setlocale("LC_TIME", "English_United States.1252") # Windows

tran %>%
  mutate(ymd = as.character(ymd)) %>%
  mutate(ymd = as.Date(ymd)) %>%
  mutate(wd = format(ymd, '%a')) %>%
  mutate(wd = factor(wd, levels =c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))) -> tran

Sys.setlocale("LC_TIME", "Korean_Korea.949") # Windows

tran %>%
  group_by(custid, wd) %>%
  summarise(cust_wd_sum = sum(amt)) -> cust_wd_sum ; head(cust_wd_sum)

# pivotting...
melted <- melt(cust_wd_sum, id.vars = c('custid', 'wd'), measure.vars = c('cust_wd_sum')) 
head(melted)
dcast(melted, custid ~ wd, value.var = 'value') -> dcasted; head(dcasted)
dcasted %>% replace(is.na(.), 0) -> cust_wd_sum.casted; head(cust_wd_sum.casted)

# instead...
id_spread_sum(cust_wd_sum)

# rowSum and ratio...
cust_wd_sum.casted %>% mutate(total.amt = rowSums(.[-1])) -> cust_wd_sum_1; head(cust_wd_sum_1)
round(cust_wd_sum_1[, -1] / cust_wd_sum_1$total.amt, 2) -> cust_wd_sum_ratio
head(cust_wd_sum_ratio)
cust_wd_sum_ratio %>% 
  select(-total.amt) -> cust_wd_sum_ratio; head(cust_wd_sum_ratio)

# var naming...
colnames(cust_wd_sum_ratio)
colnames(cust_wd_sum_ratio) <- paste0('wdf', '_', colnames(cust_wd_sum_ratio))
head(cust_wd_sum_ratio)

# cbind custid...
cust_wd_ratio <- cbind(custid = cust_wd_sum.casted[, 1], cust_wd_sum_ratio)
head(cust_wd_ratio)

tran %>%
  select(custid) %>%
  unique() %>%
  arrange(custid) %>%
  as_tibble() -> custid_df

# instead...
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

# instead...
cust_wd_sum %>% rowsum_ratio_df(., "wdf_") -> cust_wd_ratio

# purchased_prod_num 변수 : 구매한 서로 다른 제품의 수-----
head(tran)
melt(tran, id.vars = c('custid', 'prod'), measure.vars = c('amt')) -> melted
head(melted)

dcast(melted, custid ~ prod, value.var = 'value') -> dcasted; head(dcasted)
dcasted_mat <- as.matrix(dcasted)
ifelse(dcasted[, -1] > 0, 1, 0) -> dcasted_bin; dim(dcasted_bin)
rowSums(dcasted_bin)
rowSums(as.matrix(dcasted[, -1]))
data.frame(custid = dcasted$custid, 
           purchased_prod_num = rowSums(as.matrix(dcasted[, -1]))) -> purchased_df; head(purchased_df)

# instead...
tran <- read_csv('./data/transaction.csv'); head(tran)

# custid_df...
tran %>%
  select(custid) %>%
  unique() %>%
  arrange(custid) %>%
  as_tibble() -> custid_df

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

# instead...
tran %>%
  group_by(custid) %>%
  summarise(n_distinct(prod)) -> purchase_df
  
# ct_cov 변수 :제품 카테고별 구매비용 변동계수(카테고리별 구매비용의 표준편차 / 평균) ----
tran <- read_csv('./data/transaction.csv')
prod_ct_tbl <- read_csv('./data/prod_cate.csv'); head(prod_ct_tbl)
colnames(prod_ct_tbl) <- c('prod', 'prod_ct') ; head(prod_ct_tbl)
length(unique(prod_ct_tbl$prod_ct)) ## 84 --> 11

as.tibble(tran) -> tran
tran %>% left_join(prod_ct_tbl, by='prod') -> tran; head(tran)

tran %>%
  group_by(custid, prod_ct) %>%
  summarise(sum_amt = sum(amt)) %>%
  spread(prod_ct, sum_amt, fill = 0) -> tran_tmp

tran_tmp %>%
  ungroup() %>%
  select(-custid) -> tran_tmp; head(tran_tmp)

library(matrixStats)
tran_ct_mat <- as.matrix(tran_tmp)
tran_tmp$ct_mean <- rowMeans(tran_tmp); head(tran_tmp)
tran_tmp$ct_std <- matrixStats::rowSds(tran_ct_mat) ; head(tran_tmp)

tran_tmp %>% 
  select(ct_mean, ct_std) %>%
  mutate(ct_cov = ct_std/ct_mean) %>% as.data.frame() %>%
  cbind(custid = custid_df$custid, .) %>%
  select(custid, ct_cov) -> ct_cov_df; head(ct_cov_df)

# instead...
tran <- read_csv('./data/transaction.csv')
prod_ct_tbl <- read_csv('./data/prod_cate.csv')

tran %>%
  left_join(prod_ct_tbl,by = "prod") %>%
  rename(prod_ct = prod_cate) %>%
  group_by(custid, prod_ct) %>%
  summarise(sum_amt = sum(amt)) %>%
  id_spread_sum() %>%
  mutate(ct_mean = rowMeans(select_if(., is_numeric))) %>%
  mutate(ct_std = matrixStats::rowSds(as.matrix(select_if(., is_numeric)))) %>% 
  mutate(ct_cov = ct_std/ct_mean) %>%
  select(custid, ct_cov) -> ct_cov_df

# vdays 변수 : 총 구매일수----
tran <- read_csv('./data/transaction.csv')
tran %>%
  group_by(custid) %>%
  summarise(vdays = n_distinct(ymd)) -> vday_df; head(vday_df)

# day_mean_amt 변수: 하루 평균 구매 비용 ----
cust_g_paid %>% 
  left_join(vday_df, by='custid') %>%
  mutate(day_mean_amt = (g_paid/vdays)) %>%
  select(custid, day_mean_amt) -> day_mean_amt_df; head(day_mean_amt_df)

# day_cov 변수 : 일별 변동계수 :: 일일 구매비용의 `표준편차/평균` 값----
tran <- read_csv('./data/transaction.csv')
tran %>%
  group_by(custid, ymd) %>%
  summarise(sum_day_amt = sum(amt)) %>% 
  ungroup() %>%
  group_by(custid) %>%
  summarise(day_sd_amt = sd(sum_day_amt)) %>% # NaN...
  replace(is.na(.), 0) -> day_sd_amt_df; 

day_sd_amt_df %>%
  left_join(day_mean_amt_df, by='custid') %>%
  mutate(day_cov = (day_sd_amt / day_mean_amt)) %>%
  select(custid, day_cov) -> daycov_df 

head(daycov_df)

# first_cnt.prod 변수 : 가장 많은 수의 구매한 제품  ----
tran <- read_csv('./data/transaction.csv')

tran %>%
  select(custid, prod) %>%
  group_by(custid, prod) %>%
  summarise(count = n()) %>%
  arrange(desc(count), .by_group = T) %>%
  slice(1) %>%
  ungroup() %>%
  select(custid, prod) %>%
  rename(first_cnt.prod = prod) -> df.first_cnt.prod

custid_df %>% left_join(df.first_cnt.prod, by ='custid') -> df.first_cnt.prod_1

# second_count.prod 변수 : 두 번째로 많이 구매한 제품----
tran %>%
  select(custid, prod) %>%
  group_by(custid, prod) %>%
  summarise(count = n()) %>%
  arrange(desc(count), .by_group = T) %>%
  slice(2) %>%
  select(custid, prod) %>%
  rename(second_cnt.prod = prod) -> df.second_cnt.prod

df.first_cnt.prod_1 %>% left_join(df.second_cnt.prod, by = 'custid') -> df.second_cnt.prod_1

# third_count.prod 변수 : 두 번째로 많이 구매한 제품----
tran %>%
  select(custid, prod) %>%
  group_by(custid, prod) %>%
  summarise(count = n()) %>%
  arrange(desc(count), .by_group = T) %>%
  slice(3) %>%
  select(custid, prod) %>%
  rename(third_cnt.prod = prod) -> df.third_cnt.prod

df.second_cnt.prod_1 %>% left_join(df.third_cnt.prod, by = 'custid') -> df.third_cnt.prod_1

# first_money.prod_ct 변수 : 가장 많이 돈을 지출한 물품의 카테고리  ----
tran <- read_csv('./data/transaction.csv')
prod_ct_tbl <- read_csv('./data/prod_cate.csv')

tran %>%
  left_join(prod_ct_tbl,by = "prod") %>%
  rename(prod_ct = prod_cate) %>%
  group_by(custid, prod_ct) %>%
  summarise(sum.amt = sum(amt)) %>%
  arrange(desc(sum.amt), .by_group = T) %>%
  slice(1) %>%
  ungroup() %>%
  select(custid, prod_ct) %>%
  rename(first_money.prod_ct = prod_ct) -> df.first_money.prod_ct

# second_money.prod_ct 변수 : 두 번쩨로 많은 돈을 지출한 물품의 카테고리  ----
tran %>%
  left_join(prod_ct_tbl,by = "prod") %>%
  rename(prod_ct = prod_cate) %>%
  group_by(custid, prod_ct) %>%
  summarise(sum.amt = sum(amt)) %>%
  arrange(desc(sum.amt), .by_group = T) %>%
  slice(2) %>%
  ungroup() %>%
  select(custid, prod_ct) %>%
  rename(second_money.prod_ct = prod_ct) -> df.second_money.prod_ct

df.first_money.prod_ct %>% left_join(df.second_money.prod_ct, by = "custid") -> df.second_money.prod_ct_1

# third_money.prod_ct 변수 : 두 번쩨로 많은 돈을 지출한 물품의 카테고리  ----
tran %>%
  left_join(prod_ct_tbl,by = "prod") %>%
  rename(prod_ct = prod_cate) %>%
  group_by(custid, prod_ct) %>%
  summarise(sum.amt = sum(amt)) %>%
  arrange(desc(sum.amt), .by_group = T) %>%
  slice(3) %>%
  ungroup() %>%
  select(custid, prod_ct) %>%
  rename(third_money.prod_ct = prod_ct) -> df.third_money.prod_ct

df.second_money.prod_ct_1 %>% left_join(df.third_money.prod_ct, by = "custid") -> df.third_money.prod_ct_1


## ','로 고객별 구매한 물품을 하나의 row로 표현하기
tran <- read_csv('./data/transaction.csv'); head(tran)
tran %>%
  select(custid, prod) -> custid_prod_df

custid_prod_df %>%
  group_by(custid) %>%
  summarise(item_pur = paste(prod, collapse = ', ')) -> item_pur_df; head(item_pur_df)

tran %>% 
  filter(custid == 'C2048') # softdrink를 많이 구매하는 사람인지 여부 확인

item_pur_df %>%
  filter(custid == 'C2048')

## 반대로는 어떻게 할 수 있을까??


head(cust_prod_ct_ratio)

cust_prod_ct_ratio %>%
  left_join(cust_coverage, by='custid') %>%
  left_join(cust_g_paid, by='custid') %>%
  left_join(cust_buyed_prod_num, by='custid') %>%
  left_join(hf_df, by='custid') %>%
  left_join(cust_wd_ratio, by='custid') %>%
  left_join(purchase_df, by='custid') %>%
  left_join(ct_cov_df, by='custid') %>%
  left_join(vday_df, by='custid') %>%
  left_join(day_mean_amt_df, by='custid') %>%
  left_join(daycov_df, by='custid') %>%
  left_join(df.third_cnt.prod_1, by='custid') %>%
  left_join(df.third_money.prod_ct_1, by='custid')-> data_total

# 나중에 custid는 모두 factor로 변환하자.(warning)
# View(data_total)
write_csv(data_total, './data/data_total.csv') 

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

