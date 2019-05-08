#' ---
#' title: "ADP R_BASIC_2"
#' author: "jakinpilla"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::github_document
#' ---

rm(list = ls())
#+setup, include = FALSE
setwd("C:/Users/Daniel/ADP_performance_test")
getwd()

# install.packages('party')
# install.packages('TH.data')
Packages <- c('tidyverse', 'data.table', 'reshape2', 'caret', 'rpart', 'GGally', 
              'ROCR', 'randomForest', 'dummies', 'curl', 'gridExtra')

lapply(Packages, library, character.only=T)

# Binning ----------------------------------------------------------------------
library(mfp)
data(bodyfat)
glimpse(bodyfat);

bodyfat$bmi <- (bodyfat$weight*.45) / ((bodyfat$height * .02)^2)
bodyfat$bmi.bins <- cut(bodyfat$bmi, 
                        c(0,25,30,200), 
                        include.lowest = T, 
                        labels=c('normal', 'overweight', 'obese'))
bodyfat
glimpse(bodyfat); levels(bodyfat$bmi.bins)

# Time binning -----------------------------------------------------------------
tran <- read_csv('./data/transaction.csv'); tran %>% colnames()
rename(tran, hour = time) -> tran #
tran$hour <- as.numeric(substr(tran$hour, 1, 2))

tran %>% mutate(h_bin = cut(hour, 
                            breaks = c(0, 6, 12, 18, 23),
                            include.lowest = T, # to include 0 in a group...
                            labels=c('0-5', '6-11', '12-17', '18-23'))) -> tran
head(tran)
unique(tran$h_bin) # There is no '0-5' time bin...

# h_bin one-hot coding ---------------------------------------------------------
tran %>%
  select(custid, h_bin, amt) %>%
  group_by(custid, h_bin) %>%
  summarise(sum.amt = sum(amt)) %>%
  ungroup() %>%
  spread(h_bin, sum.amt, fill = 0) %>%
  mutate_if(is.numeric, funs(replace(., . > 0, 1))) # to dummies... that's it!

#' h_bin freq ratio ------------------------------------------------------------
tran %>%
  select(custid, h_bin, amt) %>%
  group_by(custid, h_bin) %>%
  summarise(freq = n())

tran %>%
  select(custid, h_bin, amt) %>%
  group_by(custid, h_bin) %>%
  summarise(freq = n()) %>%
  ungroup() %>%
  spread(h_bin, freq, fill = 0) %>%
  mutate(total_freq = rowSums(select_if(., is.numeric))) %>%
  mutate_at(vars(-custid), funs(./total_freq)) 

#' h_bin amt ratio ------------------------------------------------------------- 
tran %>%
  select(custid, h_bin, amt) %>%
  group_by(custid, h_bin) %>%
  summarise(sum.amt = sum(amt))

tran %>%
  select(custid, h_bin, amt) %>%
  group_by(custid, h_bin) %>%
  summarise(sum.amt = sum(amt)) %>%
  ungroup() %>%
  spread(h_bin, sum.amt, fill = 0) %>%
  mutate(total_sum = rowSums(select_if(., is.numeric))) %>%
  mutate_at(vars(-custid), funs(./total_sum)) 


#' CV variable : coefficient of variation, Std / Mean -----------------------------------
#' 
#' It means that the customer purchases are viased at a certain time for CV to be large...
#' 
#' Otherwise, it means that the customer purchases are not viased at a certain time for CV to be small.... That is similar with female purchase type.

tran %>%
  select(custid, h_bin, amt) %>%
  group_by(custid, h_bin) %>%
  summarise(freq = n()) %>%
  ungroup() %>%
  spread(h_bin, freq, fill = 0) %>%
  mutate(time_b_mean = apply(select(., -custid), 1, mean)) %>%
  mutate(time_b_std = apply(select(., -custid), 1, sd))
  
tran %>%
  select(custid, h_bin, amt) %>%
  group_by(custid, h_bin) %>%
  summarise(freq = n()) %>%
  ungroup() %>%
  spread(h_bin, freq, fill = 0) %>%
  mutate(time_b_mean = apply(select(., -custid), 1, mean)) %>%
  mutate(time_b_std = apply(select(., -custid), 1, sd)) %>% 
  mutate(time_b_cov = time_b_std/time_b_mean) %>%
  select(custid, time_b_cov) -> cov_df; cov_df

#' With gapminder dataset ------------------------------------------------------
# install.packages('gapminder')
library(gapminder)
data("gapminder"); glimpse(gapminder)
unique(gapminder$country)
gapminder %>% filter(country == 'Korea, Rep.' & year==2007)
gapminder %>% arrange(year, country)

#' Basic Statistics ------------------------------------------------------------
gapminder %>%
  summarise(n_obs = n(),
            n_countries = n_distinct(country),
            n_years = n_distinct(year),
            med_gdpc = median(gdpPercap),
            max_gdppc = max(gdpPercap))

#' Mutate new variables ---------------------------------------------------------
gapminder %>%
  mutate(total_gdp = pop*gdpPercap,
         le_gdp_ratio = lifeExp / gdpPercap,
         lgrk = le_gdp_ratio*100)

#' Group_by --------------------------------------------------------------------
gapminder %>%
  filter(year==2007) %>%
  group_by(continent) %>%
  summarise(n(), mean(lifeExp), median(lifeExp)) %>%
  arrange(-`median(lifeExp)`)

#' Correlations ----------------------------------------------------------------
gapminder %>% head()
summary(gapminder)
summary(gapminder$gdpPercap)
cor(gapminder$gdpPercap, gapminder$lifeExp) # 0.5837062
cor(log10(gapminder$gdpPercap), gapminder$lifeExp) # notice that the cor value increase to 0.8076179...
plot(gapminder$gdpPercap, gapminder$lifeExp, cex=.5)
plot(log10(gapminder$gdpPercap), gapminder$lifeExp, cex=.5)

gapminder %>%
  ggplot(aes(gdpPercap, lifeExp, col = country)) + geom_point() + theme(legend.position= "none")

gapminder %>%
  ggplot(aes(log10(gdpPercap), lifeExp, col = country)) + geom_point() + theme(legend.position= "none")

#' With df_imdb dataset --------------------------------------------------------
df_imdb <- read_csv('./data/imdb-5000-movie-dataset.zip'); glimpse(df_imdb)
head(df_imdb)
df_imdb$country <- as.factor(df_imdb$country); glimpse(df_imdb)

#' Like pandas .value_count() method -------------------------------------------
df_imdb %>% group_by(country) %>% tally() %>% arrange(-n)
 
#' America Movie Budget Distribution -------------------------------------------
df_imdb %>%
  filter(country == 'USA') %>%
  ggplot(aes(budget)) + geom_histogram(bins = 50, fill = 'steelblue')

#' Movie Score Mean Variation --------------------------------------------------
df_imdb %>%
  group_by(title_year) %>%
  summarise(avg_imdb_score = mean(imdb_score)) %>%
  ggplot(aes(title_year, avg_imdb_score)) + geom_point(col = "steelblue") + geom_line(linetype = 6)
  
#' Each Facter Cnt and Percentage -----------------------------------------------
data(diamonds)
diamonds %>%
  group_by(cut) %>%
  tally() %>%
  mutate(pct = round((n/sum(n))*100, 1))

#' Data Visualizatoin according to data classes ---------------------------------
#' 
# 1. One continuous Variable...
hist(gapminder$lifeExp)
hist(gapminder$gdpPercap, nclass=50)
hist(log10(gapminder$gdpPercap), nclass=50)

gapminder %>%
  ggplot(aes(gdpPercap)) + geom_histogram(bins = 50, fill = "steelblue") + scale_x_log10()

gapminder %>%
  ggplot(aes(x=gdpPercap, y=lifeExp)) + geom_point() +scale_x_log10() + geom_smooth()

gapminder %>%
  ggplot(aes(log10(gdpPercap), lifeExp)) + 
  geom_point(aes(colour = factor(country))) + geom_smooth() + theme(legend.position= "none")

#' 2. One factor Variable...
diamonds %>% ggplot(aes(cut)) + geom_bar() 

table(diamonds$cut)

prop.table(table(diamonds$cut))

round(prop.table(table(diamonds$cut)) * 100, 1)

diamonds %>%
  select(cut) %>% pull() %>% table() %>% prop.table() %>% round(2) %>% as.data.frame() %>%
  rename(cut_type = ".") %>%
  ggplot(aes(x= cut_type, y = Freq, fill = cut_type)) +
  geom_bar(stat = "identity")

library(MASS)
data(survey); glimpse(survey)
head(table(survey$W.Hnd))
chisq.test(table(survey$W.Hnd), p=c(.3, .7)) # chisq.test to figure out it follows certains distribution...
# p< .05, so it doesn't follow certain distribution

#' 3. Scatter Plot : Two more numeric variables --------------------------------
diamonds %>% sample_n(30) %>% pairs()
diamonds %>% sample_n(30) %>% ggpairs()

#' 4. Numeric Variable ~ Factor Variable :: boxplot ----------------------------
mpg %>%
  ggplot(aes(class, hwy)) + geom_boxplot()

mpg %>%
  ggplot(aes(class, hwy, color = class)) + 
  geom_boxplot(outlier.colour = "red", outlier.shape = 8) + 
  geom_jitter(col = 'gray', alpha = .5)

mpg %>% mutate(class=reorder(class, hwy, median)); mpg$class[1:5] 
#' reorder `class` column according to median value of `hwy`...

mpg %>%
  mutate(class=reorder(class, hwy, median)) %>%
  ggplot(aes(class, hwy, color = class)) + 
  geom_boxplot(outlier.colour = "red", outlier.shape = 8, alpha = .5) + 
  geom_jitter(col = 'gray')


mpg %>% 
  mutate(class=factor(class,
                      levels=c('2seater', 'subcompact', 'compact', 'midsize', 
                               'minivan', 'suv', 'pickup'))) %>% # designate x-axis order by name...
  ggplot(aes(class, hwy, color = class)) + 
  geom_boxplot(outlier.colour = "red", outlier.shape = 8, alpha = .5) + 
  geom_jitter(col = 'gray')


#' 4. Factor Variable ~ Factor Variable (xtabs() / mosaicplot()) > chisq.test() --------
data("Titanic"); glimpse(Titanic)
xtabs(~ Class + Sex + Age + Survived, data.frame(Titanic))
xtabs(Freq ~ Class + Sex + Age + Survived, data.frame(Titanic)) # Freq

as.tibble(fread('./data/titanic3.csv', data.table = F)) -> titanic; glimpse(titanic)
titanic %>% replace(is.na(.), 0) -> titanic
titanic$pclass <- as.factor(titanic$pclass)
titanic$sex <- as.factor(titanic$sex)
titanic$ticket <- as.character(titanic$ticket)
titanic$survived <- factor(titanic$survived, levels=c(0,1), labels=c('dead', 'survived'))

glimpse(titanic)

#' xtabs() ---------------------------------------------------------------------
xtabs(survived == 'survived' ~ sex + pclass, data=titanic)
xtabs(survived == 'survived' ~ sex + pclass, data=titanic) / xtabs(~ sex + pclass, data=titanic)

#' chisq.test to know whether the relationship between sex and survive is independent or not...
xtabs(~sex + survived, data = titanic)
chisq.test(xtabs(~ sex + survived, data=titanic)) # p < .05 |-> Not independent... so there is relationship.

# Mosaic plot----
mosaicplot(survived ~ pclass + sex, data = titanic, color=T) # how to interprete the mosaic plot...































