setwd("C:/Users/Daniel/ADP_performance_test")
Packages <- c('tidyverse', 'data.table', 'reshape2', 'caret', 'rpart', 'GGally', 
              'ROCR', 'party', 'randomForest')
lapply(Packages, library, character.only=T)

# Data loading----
tran <- read_csv('./data/transaction.csv'); head(tran)
prod_ct_tbl <- read_csv('./data/prod_cate.csv'); head(prod_ct_tbl)
colnames(prod_ct_tbl) <- c('prod', 'prod_ct') ; head(prod_ct_tbl)
length(unique(prod_ct_tbl$prod_ct)) 

# join between transaction data and product category table...
as.tibble(tran) -> tran
tran %>% left_join(prod_ct_tbl, by='prod') -> tran
tran %>% 
  group_by(custid, prod_ct) %>%
  summarise(sum.prod_ct = sum(amt)) -> tran_grouped; head(tran_grouped); dim(tran_grouped)

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

# rowsum_ratio_df function definition----
rowsum_ratio_df <- function(grouped_df, prefix) {
  grouped_df %>%
    id_spread_sum() %>%
    mutate(total = rowSums(select_if(., is.numeric))) %>%
    mutate_at(vars(-custid), funs(round(./total, 2))) %>%
    rename_at(vars(-custid), ~ paste0(prefix, .)) %>% select(-ncol(.)) -> df.result
  
  return(df.result)
}

# ct_xxx 변수 : 제품 카테고리별 구매비용 비율 ----
tran_grouped %>% id_spread_sum()
tran_grouped %>% rowsum_ratio_df(., "ct_") -> cust_prod_ct_ratio; cust_prod_ct_ratio

# coverage 변수 ----
tran_grouped %>%
  id_spread_sum() %>%
  mutate_at(vars(-custid), funs(ifelse(. > 0, 1, 0))) %>%
  mutate(total = rowSums(select_if(., is.numeric))) %>% 
  mutate(coverage = (total / (ncol(.) - 2))) %>% # custid, total 2개의 변수 제외
  mutate(coverage = round(coverage, 2)) %>%
  select(custid, coverage) -> cust_coverage; cust_coverage 

# g_parid : 총 구매비용 구하기----
tran %>% 
  group_by(custid) %>%
  summarise(g_paid = sum(amt)) -> cust_g_paid; cust_g_paid

# buyed_prod_num : 총 구매 제품 종류의 수 구하기----
tran %>%
  group_by(custid) %>%
  summarise(buyed_prod_num = n())  -> cust_buyed_prod_num; cust_buyed_prod_num

# hf_xxx : 기간대별 구매금액의 비율 구하기----
tran %>%
  mutate(time = as.character(time)) %>%
  mutate(time = substr(time, 1, 2)) %>%
  mutate(time = as.numeric(time)) %>%
  mutate(h_bin = cut(time, 
                     breaks = c(0, 6, 12, 18, 23),
                     include.lowest = T, # 0을 그룹에 포함 위해 필요, 아니면 NA값 반환
                     labels=c('0-5', '6-11', '12-17', '18-23'))) %>%
  group_by(custid, h_bin) %>%
  summarise(sum_paid_time_bin = sum(amt)) %>%
  rowsum_ratio_df(., "hf_") -> hf_df

# wdf_xxx :요일별 구매비용 비율 구하기----
Sys.setlocale("LC_TIME", "English_United States.1252") # Windows
tran <- read_csv('./data/transaction.csv'); head(tran)
tran %>%
  mutate(ymd = as.character(ymd)) %>%
  mutate(ymd = as.Date(ymd)) %>%
  mutate(wd = format(ymd, '%a')) %>%
  mutate(wd = factor(wd, levels =c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))) %>%
  group_by(custid, wd) %>%
  summarise(cust_wd_sum = sum(amt)) %>%
  rowsum_ratio_df(., "wdf_") -> cust_wd_ratio; cust_wd_ratio

Sys.setlocale("LC_TIME", "Korean_Korea.949") # Windows

# purchased_prod_num 변수 : 구매한 서로 다른 제품의 수-----
tran %>%
  group_by(custid) %>%
  summarise(n_distinct(prod)) -> purchase_df; purchase_df

# ct_cov 변수 :제품 카테고별 구매비용 변동계수(카테고리별 구매비용의 표준편차 / 평균) ----
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
  select(custid, ct_cov) -> ct_cov_df; ct_cov_df

# vdays 변수 : 총 구매일수----
tran %>%
  group_by(custid) %>%
  summarise(vdays = n_distinct(ymd)) -> vday_df; vday_df

# day_mean_amt 변수: 하루 평균 구매 비용 ----
cust_g_paid %>% 
  left_join(vday_df, by='custid') %>%
  mutate(day_mean_amt = (g_paid/vdays)) %>%
  select(custid, day_mean_amt) -> day_mean_amt_df; day_mean_amt_df

# day_cov 변수 : 일별 변동계수 :: 일일 구매비용의 `표준편차/평균` 값----
tran <- read_csv('./data/transaction.csv')
tran %>%
  group_by(custid, ymd) %>%
  summarise(sum_day_amt = sum(amt)) %>% 
  ungroup() %>%
  group_by(custid) %>%
  summarise(day_sd_amt = sd(sum_day_amt)) %>% # NaN...
  replace(is.na(.), 0) %>%
  left_join(day_mean_amt_df, by='custid') %>%
  mutate(day_cov = (day_sd_amt / day_mean_amt)) %>%
  select(custid, day_cov) -> daycov_df ; daycov_df

# first_cnt.prod 변수 : 가장 많은 수의 구매한 제품  ----
tran %>%
  select(custid, prod) %>%
  group_by(custid, prod) %>%
  summarise(count = n()) %>%
  arrange(desc(count), .by_group = T) %>%
  slice(1) %>%
  ungroup() %>%
  select(custid, prod) %>%
  rename(first_cnt.prod = prod) -> df.first_cnt.prod; df.first_cnt.prod

# second_count.prod 변수 : 두 번째로 많이 구매한 제품----
tran %>%
  select(custid, prod) %>%
  group_by(custid, prod) %>%
  summarise(count = n()) %>%
  arrange(desc(count), .by_group = T) %>%
  slice(2) %>%
  select(custid, prod) %>%
  rename(second_cnt.prod = prod) -> df.second_cnt.prod; df.second_cnt.prod

# third_count.prod 변수 : 두 번째로 많이 구매한 제품----
tran %>%
  select(custid, prod) %>%
  group_by(custid, prod) %>%
  summarise(count = n()) %>%
  arrange(desc(count), .by_group = T) %>%
  slice(3) %>%
  select(custid, prod) %>%
  rename(third_cnt.prod = prod) -> df.third_cnt.prod; df.third_cnt.prod

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
  rename(first_money.prod_ct = prod_ct) -> df.first_money.prod_ct; df.first_money.prod_ct

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
  rename(second_money.prod_ct = prod_ct) -> df.second_money.prod_ct; df.second_money.prod_ct

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
  rename(third_money.prod_ct = prod_ct) -> df.third_money.prod_ct; df.third_money.prod_ct

# ','로 고객별 구매한 물품을 하나의 row로 표현하기----
tran %>%
  group_by(custid) %>%
  summarise(item_pur = paste(prod, collapse = ', ')) -> item_pur_df; item_pur_df

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
  left_join(df.first_cnt.prod, by='custid') %>%
  left_join(df.second_cnt.prod, by='custid') %>%
  left_join(df.third_cnt.prod, by='custid') %>%
  left_join(df.first_money.prod_ct, by='custid') %>%
  left_join(df.second_money.prod_ct, by='custid') %>%
  left_join(df.third_money.prod_ct, by='custid') -> data_total


data_total %>% View()
