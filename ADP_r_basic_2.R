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

# data select and arange

##with gapminder dataset
# install.packages('gapminder')
library(gapminder)
data("gapminder"); glimpse(gapminder)
unique(gapminder$country)
gapminder %>% filter(country == 'Korea, Rep.' & year==2007)
gapminder %>% arrange(year, country)

## with df_imdb dataset
df_imdb %>% count(country) %>% arrange(-n)

## 미국 영화의 예산 분포 알아보기
df_imdb %>%
  filter(country == 'USA') %>%
  ggplot(aes(budget)) + geom_histogram()

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

data(diamonds)
diamonds %>%
  group_by(cut) %>%
  tally()