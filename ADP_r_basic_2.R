setwd("C:/Users/Daniel/ADP_performance_test")
getwd()

# install.packages('party')
# install.packages('TH.data')
Packages <- c('plyr', 'dplyr', 'tidyverse', 'data.table', 'reshape2', 'caret', 'rpart', 'GGally', 'ROCR', 'party', 
              'randomForest', 'dummies', 'curl', 'gridExtra')

lapply(Packages, library, character.only=T)

# make new var with bodyfat dataset by binning
# data loading
# install.packages('mfp')
library(mfp)
data(bodyfat)
glimpse(bodyfat);

bodyfat$bmi <- (bodyfat$weight*.45) / ((bodyfat$height * .02)^2)
bodyfat$bmi.bins <- cut(bodyfat$bmi, 
                        c(0,25,30,200), 
                        include.lowest = T, 
                        labels=c('normal', 'overweight', 'obese'))

glimpse(bodyfat); levels(bodyfat$bmi.bins)

# loading data
# binning bt time
tran <- read.csv('./data/transaction.csv', stringsAsFactors = F)
rename(tran, hour = time) -> tran # colnames(tran)[which(names(tran) == "time")] <- "hour"와 같은 의미
head(tran)
tran$hour <- as.numeric(substr(tran$hour, 1, 2))
glimpse(tran)

# h_bin
tran %>% mutate(h_bin = cut(hour, 
                            breaks = c(0, 6, 12, 18, 23),
                            include.lowest = T, # 0을 그룹에 포함시키기 위해 반드시 필요, 아니면 NA값 반환됨.
                            labels=c('0-5', '6-11', '12-17', '18-23'))) -> tran
head(tran)
unique(tran$h_bin) # '0-5' 시간대가 없음에 유의

# h_bin one-hot coding
tran <- dummy.data.frame(tran, names=c('h_bin'), sep='_')
colnames(tran)
head(tran,20)


# 고객별 구매시간 비율을 알아보기 위해 필요한 변수만 선택
tran %>% select(custid, `h_bin_6-11`, `h_bin_12-17`, `h_bin_18-23`) -> df_h; head(df_h)

# 고객별 구매시간 bin들의 합 구하기
df_h %>% group_by(custid) %>%
  summarise(sum.h_6_11 = sum(`h_bin_6-11`), 
            sum.h_12_17 = sum(`h_bin_12-17`),
            sum.h_18_23 = sum(`h_bin_18-23`)) -> cust_visit_h; head(cust_visit_h)

cust_visit_h %>% mutate(total_visitcount = rowSums(.[2:4])) -> cust_visit_h_total

ratio_visit_h <- round(cust_visit_h_total[, 2:4] / cust_visit_h_total$total_visitcount, 3)
head(ratio_visit_h)

dim(cust_visit_h)

ratio_cust_visit <- cbind(custid = cust_visit_h[, 1], ratio_visit_h)
head(ratio_cust_visit)

# 시간별 변동계수(coefficient of variation, CV = 표준편차 / 평균) 추가하기
# 시간별 변동계수가 크다는 것은 시간에 따른 구매가 편향되어 있다는 뜻(즉, 특정시간대에 구매함)
# 시간별 변동계수가 작다는 것은 시간대별 골고루 구매한다는 의미
# 시간대별 골고루 구매한다는 것은 여성의 소비패턴과 유사하다고 할 수 있음

# install.packages('matrixStats')
library(matrixStats)
dim(ratio_cust_visit)
head(ratio_cust_visit)

ratio_cust_visit[, -1] %>%
  mutate(row_std = round(rowSds(as.matrix(.[1:3])), 3),
         row_mean = round(rowMeans(as.matrix(.[1:3])), 3), 
         h_cv = row_std/row_mean) -> h_df

head(cust_visit_h)
head(h_df)
tran_h_df <- cbind(cust_visit_h[, 1], h_df); head(tran_h_df)

# data select and arange

##with gapminder dataset
# install.packages('gapminder')
library(gapminder)
data("gapminder"); glimpse(gapminder)
unique(gapminder$country)
gapminder %>% filter(country == 'Korea, Rep.' & year==2007)
gapminder %>% arrange(year, country)

## 요약 통계량 출력하기
gapminder %>%
  summarise(n_obs = n(),
            n_countries = n_distinct(country),
            n_years = n_distinct(year),
            med_gdpc = median(gdpPercap),
            max_gdppc = max(gdpPercap))

## 변수변환 > 컬럼추가하기(mutate())
gapminder %>%
  mutate(total_gdp = pop*gdpPercap,
         le_gdp_ratio = lifeExp / gdpPercap,
         lgrk = le_gdp_ratio*100)

# 그룹연산
gapminder %>%
  filter(year==2007) %>%
  group_by(continent) %>%
  summarise(n(), mean(lifeExp), median(lifeExp)) %>%
  arrange(-`median(lifeExp)`)

# 요약통계량, 상관관계
summary(gapminder)
summary(gapminder$gdpPercap)
cor(gapminder$gdpPercap, gapminder$lifeExp) # 0.5837062
cor(log10(gapminder$gdpPercap), gapminder$lifeExp) # 0.8076179 상관관계 증가
plot(gapminder$gdpPercap, gapminder$lifeExp, cex=.5)
plot(log10(gapminder$gdpPercap), gapminder$lifeExp, cex=.5)

## with df_imdb dataset
df_imdb <- read_csv('./data/imdb-5000-movie-dataset.zip'); glimpse(df_imdb)
head(df_imdb)
df_imdb$country <- as.factor(df_imdb$country); glimpse(df_imdb)
# like pandas .value_count() method
df_imdb %>% group_by(country) %>% tally() %>% arrange(-n)
 
## 미국 영화의 예산 분포 알아보기
df_imdb %>%
  filter(country == 'USA') %>%
  ggplot(aes(budget)) + geom_histogram()

df_imdb %>%
  group_by(title_year) %>%
  summarise(avg_imdb_score = mean(imdb_score)) %>%
  ggplot(aes(title_year, avg_imdb_score)) + geom_point() + geom_line()
  
# 각 facter 별 갯수 및 percentage 확인
data(diamonds)
diamonds %>%
  group_by(cut) %>%
  tally() %>%
  mutate(pct = round((n/sum(n))*100, 1))

# 데이터에 따른 시각화
# 1. 하나의 연속형(히스토그램)
hist(gapminder$lifeExp)
hist(gapminder$gdpPercap, nclass=50)
hist(log10(gapminder$gdpPercap), nclass=50)

gapminder %>%
  ggplot(aes(gdpPercap)) + geom_histogram() + scale_x_log10()

gapminder %>%
  ggplot(aes(x=gdpPercap, y=lifeExp)) + geom_point() +scale_x_log10() + geom_smooth()

# 2. 하나의 범주형 (막대그래프, 분할표) > 카이제곱 검정
diamonds %>% ggplot(aes(cut)) + geom_bar() # 품질별 개수
table(diamonds$cut) # 도수분포
prop.table(table(diamonds$cut))
round(prop.table(table(diamonds$cut)) * 100, 1)

library(MASS)
data(survey); glimpse(survey)
head(table(survey$W.Hnd))
chisq.test(table(survey$W.Hnd), p=c(.3, .7)) # 특정 분포를 따르는지...
# p< .05 이므로 특정 분포를 따르지 않는다.

# 3. 두 수량형 변수 (산점도)
pairs(diamonds %>% sample_n(100))
ggpairs(diamonds %>% sample_n(100))

# 4. 수량형 ~ 범주형 (박스플롯)
mpg %>%
  ggplot(aes(class, hwy)) + geom_boxplot()

mpg %>% mutate(class=reorder(class, hwy, median)); mpg$class[1:5] 
#`class` 컬럼을 hwy의 중간값(median) 순서로 정렬하여 데이터를 정렬 

mpg %>%
  mutate(class=reorder(class, hwy, median)) %>% # hwy의 중간값 순서로 정렬
  ggplot(aes(class, hwy)) + geom_jitter() + geom_boxplot(alpha=.5)

mpg %>% 
  mutate(class=factor(class,
                      levels=c('2seater', 'subcompact', 'compact', 'midsize', 
                               'minivan', 'suv', 'pickup'))) %>% ## 순서 직접 지정
  ggplot(aes(class, hwy)) + geom_jitter() + geom_boxplot(alpha=.5)

# 4. 범주형 ~ 범주형 (분할표 / 모자이크) > 카이제곱 검정
data("Titanic"); glimpse(Titanic)
xtabs(~ Class + Sex + Age + Survived, data.frame(Titanic))
xtabs(Freq ~ Class + Sex + Age + Survived, data.frame(Titanic)) # Freq를 알 수 있음

as.tibble(fread('./data/titanic3.csv', data.table = F)) -> titanic; glimpse(titanic)
titanic %>% replace(is.na(.), 0) -> titanic
titanic$pclass <- as.factor(titanic$pclass)
titanic$sex <- as.factor(titanic$sex)
titanic$ticket <- as.character(titanic$ticket)
titanic$survived <- factor(titanic$survived, levels=c(0,1), labels=c('dead', 'survived'))

glimpse(titanic)

# 분할표--
xtabs(survived == 'survived' ~ sex + pclass, data=titanic)
xtabs(survived == 'survived' ~ sex + pclass, data=titanic) / xtabs(~ sex + pclass, data=titanic)

# 성별과 사망률이 독립인지?
chisq.test(xtabs(~ sex + survived, data=titanic)) # p < .05 |-> 독립이 아니다. 연관관계가 있다.

# mosaic plot----
mosaicplot(survived ~ pclass + sex, data = titanic, color=T) # 해석에 집중해보자































