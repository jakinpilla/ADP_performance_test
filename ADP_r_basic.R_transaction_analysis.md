ADP R Basic(transaction data analysis)
================
jakinpilla
May 3rd, 2014

setting working dtrectory 거래 데이터 분석 거래 데이터 분석을 위한 고객별 파생변수를 어떻게 생성하는지를
알아온다. 이번 장의 목적은 일종의 고객 데이터 feature engineering이라고 할 수 있다.

일단 분석에 필요한 여러 개의 패키지를 한 번에
읽는다.

``` r
Packages <- c('tidyverse', 'data.table', 'reshape2', 'caret', 'rpart', 'GGally', 
              'ROCR', 'party', 'randomForest', 'knitr')
lapply(Packages, library, character.only=T)
```

    ## [[1]]
    ##  [1] "forcats"   "stringr"   "dplyr"     "purrr"     "readr"    
    ##  [6] "tidyr"     "tibble"    "ggplot2"   "tidyverse" "stats"    
    ## [11] "graphics"  "grDevices" "utils"     "datasets"  "methods"  
    ## [16] "base"     
    ## 
    ## [[2]]
    ##  [1] "data.table" "forcats"    "stringr"    "dplyr"      "purrr"     
    ##  [6] "readr"      "tidyr"      "tibble"     "ggplot2"    "tidyverse" 
    ## [11] "stats"      "graphics"   "grDevices"  "utils"      "datasets"  
    ## [16] "methods"    "base"      
    ## 
    ## [[3]]
    ##  [1] "reshape2"   "data.table" "forcats"    "stringr"    "dplyr"     
    ##  [6] "purrr"      "readr"      "tidyr"      "tibble"     "ggplot2"   
    ## [11] "tidyverse"  "stats"      "graphics"   "grDevices"  "utils"     
    ## [16] "datasets"   "methods"    "base"      
    ## 
    ## [[4]]
    ##  [1] "caret"      "lattice"    "reshape2"   "data.table" "forcats"   
    ##  [6] "stringr"    "dplyr"      "purrr"      "readr"      "tidyr"     
    ## [11] "tibble"     "ggplot2"    "tidyverse"  "stats"      "graphics"  
    ## [16] "grDevices"  "utils"      "datasets"   "methods"    "base"      
    ## 
    ## [[5]]
    ##  [1] "rpart"      "caret"      "lattice"    "reshape2"   "data.table"
    ##  [6] "forcats"    "stringr"    "dplyr"      "purrr"      "readr"     
    ## [11] "tidyr"      "tibble"     "ggplot2"    "tidyverse"  "stats"     
    ## [16] "graphics"   "grDevices"  "utils"      "datasets"   "methods"   
    ## [21] "base"      
    ## 
    ## [[6]]
    ##  [1] "GGally"     "rpart"      "caret"      "lattice"    "reshape2"  
    ##  [6] "data.table" "forcats"    "stringr"    "dplyr"      "purrr"     
    ## [11] "readr"      "tidyr"      "tibble"     "ggplot2"    "tidyverse" 
    ## [16] "stats"      "graphics"   "grDevices"  "utils"      "datasets"  
    ## [21] "methods"    "base"      
    ## 
    ## [[7]]
    ##  [1] "ROCR"       "gplots"     "GGally"     "rpart"      "caret"     
    ##  [6] "lattice"    "reshape2"   "data.table" "forcats"    "stringr"   
    ## [11] "dplyr"      "purrr"      "readr"      "tidyr"      "tibble"    
    ## [16] "ggplot2"    "tidyverse"  "stats"      "graphics"   "grDevices" 
    ## [21] "utils"      "datasets"   "methods"    "base"      
    ## 
    ## [[8]]
    ##  [1] "party"       "strucchange" "sandwich"    "zoo"         "modeltools" 
    ##  [6] "stats4"      "mvtnorm"     "grid"        "ROCR"        "gplots"     
    ## [11] "GGally"      "rpart"       "caret"       "lattice"     "reshape2"   
    ## [16] "data.table"  "forcats"     "stringr"     "dplyr"       "purrr"      
    ## [21] "readr"       "tidyr"       "tibble"      "ggplot2"     "tidyverse"  
    ## [26] "stats"       "graphics"    "grDevices"   "utils"       "datasets"   
    ## [31] "methods"     "base"       
    ## 
    ## [[9]]
    ##  [1] "randomForest" "party"        "strucchange"  "sandwich"    
    ##  [5] "zoo"          "modeltools"   "stats4"       "mvtnorm"     
    ##  [9] "grid"         "ROCR"         "gplots"       "GGally"      
    ## [13] "rpart"        "caret"        "lattice"      "reshape2"    
    ## [17] "data.table"   "forcats"      "stringr"      "dplyr"       
    ## [21] "purrr"        "readr"        "tidyr"        "tibble"      
    ## [25] "ggplot2"      "tidyverse"    "stats"        "graphics"    
    ## [29] "grDevices"    "utils"        "datasets"     "methods"     
    ## [33] "base"        
    ## 
    ## [[10]]
    ##  [1] "knitr"        "randomForest" "party"        "strucchange" 
    ##  [5] "sandwich"     "zoo"          "modeltools"   "stats4"      
    ##  [9] "mvtnorm"      "grid"         "ROCR"         "gplots"      
    ## [13] "GGally"       "rpart"        "caret"        "lattice"     
    ## [17] "reshape2"     "data.table"   "forcats"      "stringr"     
    ## [21] "dplyr"        "purrr"        "readr"        "tidyr"       
    ## [25] "tibble"       "ggplot2"      "tidyverse"    "stats"       
    ## [29] "graphics"     "grDevices"    "utils"        "datasets"    
    ## [33] "methods"      "base"

일단 거래데이터를 아래와 같이 불러온다.

``` r
tran <- read_csv("data/transaction.csv"); head(tran)
```

    ## Parsed with column specification:
    ## cols(
    ##   ymd = col_date(format = ""),
    ##   time = col_time(format = ""),
    ##   custid = col_character(),
    ##   prod = col_character(),
    ##   amt = col_double()
    ## )

    ## # A tibble: 6 x 5
    ##   ymd        time   custid prod               amt
    ##   <date>     <time> <chr>  <chr>            <dbl>
    ## 1 2012-04-01 10:00  C2048  softdrinks         700
    ## 2 2012-04-01 10:00  C2069  fermentedmilk     2990
    ## 3 2012-04-01 10:00  C2069  chilledlivestock  5980
    ## 4 2012-04-01 10:00  C2069  cooky/cakes       3200
    ## 5 2012-04-01 10:00  C2069  mushrooms         1000
    ## 6 2012-04-01 10:00  C0211  bread             5200

## 제품 카테고리별 구매비용 비율 :: ct\_xxx 파생변수 만들기

고객이 각 물품 카테고리별로 지출한 총금액의 비율을 알아보자.

``` r
length(unique(tran$prod))
```

    ## [1] 84

instead…

``` r
tran %>%
  select(prod) %>%
  summarise(dict_count =n_distinct(prod))
```

    ## # A tibble: 1 x 1
    ##   dict_count
    ##        <int>
    ## 1         84

물품종류의 총 갯수는 84개이다.

like python value.count() :: 각 물품별 거래데이터에서 등장횟수는 다음과 같이 손쉽게 구할 수 있다.

``` r
tran %>%
  select(prod) %>%
  group_by(prod) %>%
  tally() %>% arrange(desc(n))
```

    ## # A tibble: 84 x 2
    ##    prod                n
    ##    <chr>           <int>
    ##  1 softdrinks       1418
    ##  2 milk             1414
    ##  3 fruitvegetables  1075
    ##  4 fermentedmilk    1069
    ##  5 beans            1061
    ##  6 harddrinks        942
    ##  7 ramen             928
    ##  8 snacks            907
    ##  9 cooky/cakes       892
    ## 10 eidbleherbs       801
    ## # … with 74 more rows

위의 실행결과에서 알 수 있듯이 softdrink 가 가장 많이 거래되는 물품이다.

### Load prod\_cate.csv and join the tbl

각 물품이 어느 카테고리에 해당하는가에 대한 테이블을 불러온다.

``` r
prod_ct_tbl <- read.csv('./data/prod_cate.csv', stringsAsFactors = F)
colnames(prod_ct_tbl) <- c('prod', 'prod_ct') ; head(prod_ct_tbl)
```

    ##               prod   prod_ct
    ## 1       softdrinks     drink
    ## 2    fermentedmilk     dairy
    ## 3 chilledlivestock      meat
    ## 4      cooky/cakes     snack
    ## 5        mushrooms vegetable
    ## 6            bread     snack

``` r
length(unique(prod_ct_tbl$prod_ct)) ## 84 --> 11
```

    ## [1] 11

거래데이터에서 각 물품들은 11개의 카테고리로 분류된다.

joining :: 위의 테이블을 원래 거래 데이터와 조인한다.

``` r
as_tibble(tran) -> tran
tran %>% left_join(prod_ct_tbl, by='prod') -> tran
tran
```

    ## # A tibble: 27,993 x 6
    ##    ymd        time   custid prod                  amt prod_ct   
    ##    <date>     <time> <chr>  <chr>               <dbl> <chr>     
    ##  1 2012-04-01 10:00  C2048  softdrinks            700 drink     
    ##  2 2012-04-01 10:00  C2069  fermentedmilk        2990 dairy     
    ##  3 2012-04-01 10:00  C2069  chilledlivestock     5980 meat      
    ##  4 2012-04-01 10:00  C2069  cooky/cakes          3200 snack     
    ##  5 2012-04-01 10:00  C2069  mushrooms            1000 vegetable 
    ##  6 2012-04-01 10:00  C0211  bread                5200 snack     
    ##  7 2012-04-01 10:00  C0859  laundrydetergent     2000 house_prod
    ##  8 2012-04-01 11:00  C0819  beans                1950 vegetable 
    ##  9 2012-04-01 11:00  C0819  mushrooms            1000 vegetable 
    ## 10 2012-04-01 11:00  C1321  otherreadymadefoods  2000 fastfood  
    ## # … with 27,983 more rows

위와 같이 조인된 결과를 확인할 수 있다.

우리는 고객별, 카테고리별 총 지출금의 비율을 알아보는 것이므로 custid와 prod\_ct를 group\_by하고 각 그룸별
지출 총액을 다음과 같이 구한다.

``` r
tran %>% 
  group_by(custid, prod_ct) %>%
  summarise(sum.prod_ct = sum(amt)) -> tran_grouped; head(tran_grouped); dim(tran_grouped)
```

    ## # A tibble: 6 x 3
    ## # Groups:   custid [2]
    ##   custid prod_ct    sum.prod_ct
    ##   <chr>  <chr>            <dbl>
    ## 1 C0001  dairy             2300
    ## 2 C0001  drink             3540
    ## 3 C0002  dairy              990
    ## 4 C0002  fastfood          9990
    ## 5 C0002  fruite           21800
    ## 6 C0002  house_prod        4900

    ## [1] 9440    3

### Pivotting

Pivotting을 통해 일단 각 고객별, 카테고리별 총 지출금액에 대한 데이터를
확보한다.

``` r
molten <- melt(tran_grouped, id.vars=c('custid', 'prod_ct'), measure.vars = c('sum.prod_ct'))
dcasted <- dcast(molten, custid ~ prod_ct, value.var = 'value') -> dcasted
dcasted %>% replace(is.na(.), 0) -> cust_prod_ct_amt; 
head(cust_prod_ct_amt) %>% kable()
```

| custid | dairy | drink | fastfood | fruite | house\_prod | makeup |  meat | sauce | seafood | snack | vegetable |
| :----- | ----: | ----: | -------: | -----: | ----------: | -----: | ----: | ----: | ------: | ----: | --------: |
| C0001  |  2300 |  3540 |        0 |      0 |           0 |      0 |     0 |     0 |       0 |     0 |         0 |
| C0002  |   990 |     0 |     9990 |  21800 |        4900 |      0 | 50638 |     0 |       0 |     0 |      1850 |
| C0003  |     0 |  1000 |        0 |      0 |           0 |      0 |  4950 |  2250 |       0 |  1780 |         0 |
| C0004  | 14780 | 39300 |    30940 |  23830 |        2050 |      0 | 34125 |  9290 |   14100 |     0 |     33131 |
| C0005  |  2600 |  1420 |    11800 |   3490 |       33600 |      0 | 27073 |  4390 |    3150 |  2610 |      3560 |
| C0006  |     0 |     0 |     3980 |      0 |           0 |      0 |     0 |     0 |       0 |     0 |         0 |

### Rowsum and Ratio

이제는 각 카테고리별 지출 금액의 비율을 알아보기 위해 위의 데이터를 각각의 고객이 지출한 지출금액으로 나눈다.

이를 위해 고객별 총 지출금액은 다음과 같이 구할 수 있다.

``` r
cust_prod_ct_amt %>%
  mutate(total.amt = rowSums(.[-1])) -> cust_prod_ct_sum; head(cust_prod_ct_sum)
```

    ##   custid dairy drink fastfood fruite house_prod makeup  meat sauce seafood
    ## 1  C0001  2300  3540        0      0          0      0     0     0       0
    ## 2  C0002   990     0     9990  21800       4900      0 50638     0       0
    ## 3  C0003     0  1000        0      0          0      0  4950  2250       0
    ## 4  C0004 14780 39300    30940  23830       2050      0 34125  9290   14100
    ## 5  C0005  2600  1420    11800   3490      33600      0 27073  4390    3150
    ## 6  C0006     0     0     3980      0          0      0     0     0       0
    ##   snack vegetable total.amt
    ## 1     0         0      5840
    ## 2     0      1850     90168
    ## 3  1780         0      9980
    ## 4     0     33131    201546
    ## 5  2610      3560     93693
    ## 6     0         0      3980

이제 총 지출금액으로 데이터를 나누어 `ratio`를 구한다. ratio를 구할때 소수점 아랫자리 2째 자리까지 계산하여
사용한다. 계산을 위해 가장 첫 번째 컬럼인 custid는 제외시킨다.(`cust_prod_ct_amt[,
-1]`)

``` r
round(cust_prod_ct_amt[, -1] / cust_prod_ct_sum$total.amt, 2) -> cust_prod_ct_ratio
cust_prod_ct_ratio[1:10, 1:10]
```

    ##    dairy drink fastfood fruite house_prod makeup meat sauce seafood snack
    ## 1   0.39  0.61     0.00   0.00       0.00      0 0.00  0.00    0.00  0.00
    ## 2   0.01  0.00     0.11   0.24       0.05      0 0.56  0.00    0.00  0.00
    ## 3   0.00  0.10     0.00   0.00       0.00      0 0.50  0.23    0.00  0.18
    ## 4   0.07  0.19     0.15   0.12       0.01      0 0.17  0.05    0.07  0.00
    ## 5   0.03  0.02     0.13   0.04       0.36      0 0.29  0.05    0.03  0.03
    ## 6   0.00  0.00     1.00   0.00       0.00      0 0.00  0.00    0.00  0.00
    ## 7   0.38  0.00     0.12   0.02       0.00      0 0.00  0.08    0.24  0.00
    ## 8   0.61  0.39     0.00   0.00       0.00      0 0.00  0.00    0.00  0.00
    ## 9   0.07  0.08     0.24   0.00       0.00      0 0.41  0.06    0.00  0.14
    ## 10  0.74  0.00     0.00   0.09       0.00      0 0.00  0.00    0.00  0.00

다시 고객 아이디를 결합하여 데이터를 만들고, 생성된 데이터의 변수의 이름을 `ct_xx` 형식으로 만들어 데이터를
완성한다.

``` r
cust_prod_ct_ratio <- cbind(custid = cust_prod_ct_amt[, 1], cust_prod_ct_ratio)
colnames(cust_prod_ct_ratio) <- c('custid', paste0('ct', '_', colnames(cust_prod_ct_ratio[, -1])))
head(cust_prod_ct_ratio) %>% kable()
```

| custid | ct\_dairy | ct\_drink | ct\_fastfood | ct\_fruite | ct\_house\_prod | ct\_makeup | ct\_meat | ct\_sauce | ct\_seafood | ct\_snack | ct\_vegetable |
| :----- | --------: | --------: | -----------: | ---------: | --------------: | ---------: | -------: | --------: | ----------: | --------: | ------------: |
| C0001  |      0.39 |      0.61 |         0.00 |       0.00 |            0.00 |          0 |     0.00 |      0.00 |        0.00 |      0.00 |          0.00 |
| C0002  |      0.01 |      0.00 |         0.11 |       0.24 |            0.05 |          0 |     0.56 |      0.00 |        0.00 |      0.00 |          0.02 |
| C0003  |      0.00 |      0.10 |         0.00 |       0.00 |            0.00 |          0 |     0.50 |      0.23 |        0.00 |      0.18 |          0.00 |
| C0004  |      0.07 |      0.19 |         0.15 |       0.12 |            0.01 |          0 |     0.17 |      0.05 |        0.07 |      0.00 |          0.16 |
| C0005  |      0.03 |      0.02 |         0.13 |       0.04 |            0.36 |          0 |     0.29 |      0.05 |        0.03 |      0.03 |          0.04 |
| C0006  |      0.00 |      0.00 |         1.00 |       0.00 |            0.00 |          0 |     0.00 |      0.00 |        0.00 |      0.00 |          0.00 |

### Pivotting and calculating ratio with dplyr more easily…

이번에는 pivottig을 보다 쉽게하는 다른 방법에 대해 알아보자.

``` r
head(tran_grouped)
```

    ## # A tibble: 6 x 3
    ## # Groups:   custid [2]
    ##   custid prod_ct    sum.prod_ct
    ##   <chr>  <chr>            <dbl>
    ## 1 C0001  dairy             2300
    ## 2 C0001  drink             3540
    ## 3 C0002  dairy              990
    ## 4 C0002  fastfood          9990
    ## 5 C0002  fruite           21800
    ## 6 C0002  house_prod        4900

위의 데이터에서 `dplyr`의 `spread()` 함수를 이용하면 상당히 편리하게 데이터 변형이 가능하다. 시험장에서는 가급적
이 방법으로 데이터를 전처리한다.

``` r
tran_grouped %>%
  spread(prod_ct, value = sum.prod_ct) %>%
  replace(is.na(.), 0) %>% as.data.frame() -> trans_grouped_df
head(trans_grouped_df)
```

    ##   custid dairy drink fastfood fruite house_prod makeup  meat sauce seafood
    ## 1  C0001  2300  3540        0      0          0      0     0     0       0
    ## 2  C0002   990     0     9990  21800       4900      0 50638     0       0
    ## 3  C0003     0  1000        0      0          0      0  4950  2250       0
    ## 4  C0004 14780 39300    30940  23830       2050      0 34125  9290   14100
    ## 5  C0005  2600  1420    11800   3490      33600      0 27073  4390    3150
    ## 6  C0006     0     0     3980      0          0      0     0     0       0
    ##   snack vegetable
    ## 1     0         0
    ## 2     0      1850
    ## 3  1780         0
    ## 4     0     33131
    ## 5  2610      3560
    ## 6     0         0

추후 고객 아이디를 조인하는 연산이 많이 등장하므로 고객아이디만을 컬럼으로 가지는 data.frame()를
정의해두자.

``` r
data.frame(custid = trans_grouped_df$custid) -> custid_df; head(custid_df)
```

    ##   custid
    ## 1  C0001
    ## 2  C0002
    ## 3  C0003
    ## 4  C0004
    ## 5  C0005
    ## 6  C0006

고객 아이디를 제외하여 두고 각 고객이 지출한 금액의 총합을 구해 마지막 열에 생성하는 코드를 다음과 같이 작성할 수 있다.

``` r
trans_grouped_df %>%  
  select(-custid) %>%
  mutate(total.sum = rowSums(.)) -> tran_g_spread; head(tran_g_spread)
```

    ##   dairy drink fastfood fruite house_prod makeup  meat sauce seafood snack
    ## 1  2300  3540        0      0          0      0     0     0       0     0
    ## 2   990     0     9990  21800       4900      0 50638     0       0     0
    ## 3     0  1000        0      0          0      0  4950  2250       0  1780
    ## 4 14780 39300    30940  23830       2050      0 34125  9290   14100     0
    ## 5  2600  1420    11800   3490      33600      0 27073  4390    3150  2610
    ## 6     0     0     3980      0          0      0     0     0       0     0
    ##   vegetable total.sum
    ## 1         0      5840
    ## 2      1850     90168
    ## 3         0      9980
    ## 4     33131    201546
    ## 5      3560     93693
    ## 6         0      3980

위의 데이터 프레임 형식의 데이터를 `matrix` 형식으로 변환하고 이를 총합으로 나누어 소수점 2짜자리까지 남겨둔 다음 다시
데이터 프레임 형식으로 변환한다. 그리고 마지막 열인 \`total.sum\`\` 컬럼은 제거한다.

``` r
(as.matrix(tran_g_spread) / tran_g_spread$total.sum) %>%
  round(2) %>% as.data.frame() %>%
  select(-total.sum) -> prod_ct_ratio; head(prod_ct_ratio)
```

    ##   dairy drink fastfood fruite house_prod makeup meat sauce seafood snack
    ## 1  0.39  0.61     0.00   0.00       0.00      0 0.00  0.00    0.00  0.00
    ## 2  0.01  0.00     0.11   0.24       0.05      0 0.56  0.00    0.00  0.00
    ## 3  0.00  0.10     0.00   0.00       0.00      0 0.50  0.23    0.00  0.18
    ## 4  0.07  0.19     0.15   0.12       0.01      0 0.17  0.05    0.07  0.00
    ## 5  0.03  0.02     0.13   0.04       0.36      0 0.29  0.05    0.03  0.03
    ## 6  0.00  0.00     1.00   0.00       0.00      0 0.00  0.00    0.00  0.00
    ##   vegetable
    ## 1      0.00
    ## 2      0.02
    ## 3      0.00
    ## 4      0.16
    ## 5      0.04
    ## 6      0.00

위에서 생성된 데이터의 컬럼이름을 `ct_xx` 형식으로
변환한다.

``` r
colnames(prod_ct_ratio) <- paste0('ct_', colnames(prod_ct_ratio)); head(prod_ct_ratio)
```

    ##   ct_dairy ct_drink ct_fastfood ct_fruite ct_house_prod ct_makeup ct_meat
    ## 1     0.39     0.61        0.00      0.00          0.00         0    0.00
    ## 2     0.01     0.00        0.11      0.24          0.05         0    0.56
    ## 3     0.00     0.10        0.00      0.00          0.00         0    0.50
    ## 4     0.07     0.19        0.15      0.12          0.01         0    0.17
    ## 5     0.03     0.02        0.13      0.04          0.36         0    0.29
    ## 6     0.00     0.00        1.00      0.00          0.00         0    0.00
    ##   ct_sauce ct_seafood ct_snack ct_vegetable
    ## 1     0.00       0.00     0.00         0.00
    ## 2     0.00       0.00     0.00         0.02
    ## 3     0.23       0.00     0.18         0.00
    ## 4     0.05       0.07     0.00         0.16
    ## 5     0.05       0.03     0.03         0.04
    ## 6     0.00       0.00     0.00         0.00

마지막으로 고객아이디 데이터프레임과 결합하여 최종 데이터를 완성한다.

``` r
ct_ratio_df <- cbind(custid = custid_df$custid, 
                     prod_ct_ratio)

head(ct_ratio_df) %>% kable()
```

| custid | ct\_dairy | ct\_drink | ct\_fastfood | ct\_fruite | ct\_house\_prod | ct\_makeup | ct\_meat | ct\_sauce | ct\_seafood | ct\_snack | ct\_vegetable |
| :----- | --------: | --------: | -----------: | ---------: | --------------: | ---------: | -------: | --------: | ----------: | --------: | ------------: |
| C0001  |      0.39 |      0.61 |         0.00 |       0.00 |            0.00 |          0 |     0.00 |      0.00 |        0.00 |      0.00 |          0.00 |
| C0002  |      0.01 |      0.00 |         0.11 |       0.24 |            0.05 |          0 |     0.56 |      0.00 |        0.00 |      0.00 |          0.02 |
| C0003  |      0.00 |      0.10 |         0.00 |       0.00 |            0.00 |          0 |     0.50 |      0.23 |        0.00 |      0.18 |          0.00 |
| C0004  |      0.07 |      0.19 |         0.15 |       0.12 |            0.01 |          0 |     0.17 |      0.05 |        0.07 |      0.00 |          0.16 |
| C0005  |      0.03 |      0.02 |         0.13 |       0.04 |            0.36 |          0 |     0.29 |      0.05 |        0.03 |      0.03 |          0.04 |
| C0006  |      0.00 |      0.00 |         1.00 |       0.00 |            0.00 |          0 |     0.00 |      0.00 |        0.00 |      0.00 |          0.00 |

## Coverage 파생변수

서로 다른 카테고리의 제품을 얼마나 다양하게 구매하였는지에 대한 비율을 구한다. 한 고객이 물건을 다양하게 구매하였는지 아니면 몇
안 되는 물품만을 구매하였는지에 대한 변수이다. 흔히 여성 고객의 경우 물품을 다양하게 구매하는 반면 남성 고객은 물품을 편협하게
구매하는 경향이
    있다.

``` r
head(trans_grouped_df)
```

    ##   custid dairy drink fastfood fruite house_prod makeup  meat sauce seafood
    ## 1  C0001  2300  3540        0      0          0      0     0     0       0
    ## 2  C0002   990     0     9990  21800       4900      0 50638     0       0
    ## 3  C0003     0  1000        0      0          0      0  4950  2250       0
    ## 4  C0004 14780 39300    30940  23830       2050      0 34125  9290   14100
    ## 5  C0005  2600  1420    11800   3490      33600      0 27073  4390    3150
    ## 6  C0006     0     0     3980      0          0      0     0     0       0
    ##   snack vegetable
    ## 1     0         0
    ## 2     0      1850
    ## 3  1780         0
    ## 4     0     33131
    ## 5  2610      3560
    ## 6     0         0

일단 위의 데이터에서 0이 아닌 컬럼의 수를 구한다. 이를 구하기 위해 데이터를 matrix 형식으로 변환 후 cust\_id를
제거하고 구매를 했으면 1 하지 않았으면 0인 데이터를 만들어본다.

``` r
trans_grouped_mat <- as.matrix(trans_grouped_df[, -1])
trans_grouped_mat_bin <- ifelse(trans_grouped_mat[,] > 0, 1, 0) # 구매했으면 1, 안 했으면 0
```

위의 데이터를 가로방향으로 더한 후 합계를 전체 카테고리 수로 나누면 각각의 고객이 얼마나 다양한 제품을 구매하였는지를 알 수
있는 데이터를 `vector` 형식으로 구할 수
있다.

``` r
round(rowSums(trans_grouped_mat_bin) / 11, 2) -> vec.coverage  # 11개의 카테고리이므로 11로 나눔
vec.coverage[1:10]; length(vec.coverage)
```

    ##  [1] 0.18 0.55 0.36 0.82 0.91 0.09 0.55 0.18 0.55 0.27

    ## [1] 2089

이렇게 구한 `vector` 데이터를 `custid_df`에 `mutate()` 한다.

``` r
custid_df %>%
  mutate(coverage = vec.coverage) -> df.coverage
head(df.coverage)
```

    ##   custid coverage
    ## 1  C0001     0.18
    ## 2  C0002     0.55
    ## 3  C0003     0.36
    ## 4  C0004     0.82
    ## 5  C0005     0.91
    ## 6  C0006     0.09

위의 과정을 보다 편리하게 할 수 있는 방법은 없을까?

``` r
ncol.num <- ncol(trans_grouped_df)-1 # 먼저 추후 나누게될 카테고리의 수를 구해 저장해 놓는다.

trans_grouped_df %>%
  select(-custid) %>%
  mutate_all(~ replace(., . > 0, 1)) %>% # mutate_all(~replace(., "condition", value)) 형식을 이용하면 
  # 특정조건을 만족하는 데이터프레임의 전 원소를 특정한 값으로 한 번에 변환할 수 있다.
  mutate(total.sum = rowSums(.)) %>%
  mutate(coverage = total.sum/ ncol.num) %>% # 전체 카테고리수로 total.sum을 나눈다.
  mutate(coverage = round(coverage, 2)) %>%
  select(coverage) -> coverage_df; head(coverage_df)
```

    ##   coverage
    ## 1     0.18
    ## 2     0.55
    ## 3     0.36
    ## 4     0.82
    ## 5     0.91
    ## 6     0.09

``` r
cbind(custid_df, coverage_df) -> coverage_df; head(coverage_df)
```

    ##   custid coverage
    ## 1  C0001     0.18
    ## 2  C0002     0.55
    ## 3  C0003     0.36
    ## 4  C0004     0.82
    ## 5  C0005     0.91
    ## 6  C0006     0.09
