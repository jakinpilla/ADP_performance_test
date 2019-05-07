#' ---
#' title: "ADP R_BASIC_1"
#' author: "jakinpilla"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::github_document
#' ---

#+setup, include = FALSE
setwd("C:/Users/Daniel/ADP_performance_test")
# setwd("/home/insa/ADP_performance_test")
getwd()
Packages <- c('plyr', 'dplyr', 'tidyverse', 'data.table', 'reshape2', 'caret', 'rpart', 'GGally', 'ROCR', 
              'randomForest', 'dummies', 'curl', 'gridExtra')
lapply(Packages, library, character.only=T)


#' Loading housing data -----------------------------------------------------------------------------
read.table('./data/housing_data.csv') -> boston
names(boston) <- c('crim', 'zn', 'indus', 'chas', 'nox', 'rm', 'age', 'dis', 'rad',  
                   'tax', 'ptratio', 'black', 'lstat', 'medv')
head(boston)
# crim :: per capita crime rate by town
# zn :: proportion of residential land zoned for lots over 25,000 sq.ft.
# indus :: proportion of non-retail business acres per town
# chas :: charles river dummy variable (=1 if tract bounds river; 0 otherwise)
# nox :: nitric oxides concentration (parts per 10 million)
# rm :: average number of rooms per dwelling
# age :: proportion of owner-occupied units built prior to 1940
# dis :: weighted distance to five Boston employment centers
# rad :: index of accessibility to radial highways
# tax :: full-value property-tax rate per $10,000
# ptratio :: pupil-teacher ratio by town
# black :: 1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town
# lstat :: % lower status of the population
# medv :: Median value of owner-occupied homes in $1000's

#' Reading various file types --------------------------------------------------------------------------
read.table('./data/students.txt', sep='\t', header=F) -> student ; head(student) # If there is no header...
d1 <- read.table("./data/student-mat.csv",sep=";",header=TRUE) ; head(d1) # deliminator is ";"

student <- read_delim("data/student.txt",  "\t", col_names = FALSE); student # delim is "tap" and no header...
student1 <- read_delim("data/student1.txt", "\t"); student1 # delim is "tap" and there is header...
student2 <- read_delim("data/student2.txt", ";"); student2  # delim is ";" and there is header...

#' In case when NA has certain character...
student3 <- read_delim("data/student3.txt", "\t", na= '-') # converet '-' to NA...

#' strip.white ---------------------------------------------------------------------------
student4 <- read_csv("data/student4.txt", col_names = FALSE); head(student4)
white_wine <- read.table('./data/winequality-white.csv', strip.white = F, sep=';', 
                         header=T); head(white_wine)

white_wine <- read.table('./data/winequality-white.csv',sep=';', 
                         header=T); head(white_wine)
#' na.rm=T ---------------------------------------------------------------------------
student3
mean(student3$키)
mean(student3$키, na.rm=T)

#' EDA basic (glimpse, plot(numeric_var ~ factor_var, data))
# summary(boston)
plot(boston[, c('crim', 'zn', 'indus', 'chas', 'black', 'lstat', 'medv')])
ggpairs(boston[, c('crim', 'zn', 'indus', 'chas', 'black', 'lstat', 'medv')])
plot(boston$crim) # if one numeric var, index on axis-x

#' iris ---------------------------------------------------------------------------
plot(iris$Sepal.Length)
plot(iris$Species) # only one factor var, factor on x-axis , frequency on y-axis
plot(Species ~ Sepal.Length, data=iris) # factor on LHS, numeric on RHS => mosaic plot
plot(Sepal.Length ~ Species, data=iris) # numericl on LHS , factor on RHS ==> boxplot

iris %>%
  ggplot(aes(Species,Sepal.Length, col = Species)) + 
  geom_boxplot() + 
  geom_point() + geom_jitter()

iris %>%
  ggplot(aes(Sepal.Length, Sepal.Width, col = Species)) + geom_point()

#' GGally::ggpairs() ---------------------------------------------------------------------------
pairs(iris[, 1:4])
iris %>% select_if(is.numeric) %>% ggpairs

#' Correlations Matrix ---------------------------------------------------------------------------
cor(iris[, 1:4])
round(cor(iris[, 1:4]), 2)
round(cor(iris[, 1:4]), 1)


#' Titanic ----
#' 
#' Convert whitespace("") value into NA ---------------------------------------------------------------------------
tbl_df(fread('./data/titanic3.csv', data.table = F)) -> titanic
# summary(titanic)
titanic$cabin <- ifelse(titanic$cabin == "", NA, titanic$cabin)
titanic$cabin[1:10] 

#' Replace all "" into NA notation ---------------------------------------------
titanic %>%
  mutate_all(funs(ifelse(. == "", NA, .)))

#' Replace all NA values into 0 -----------------------------------------
titanic %>% replace(is.na(.), 0) -> titanic_na_zero_replaced; titanic_na_zero_replaced

#' Replace all "" values into "NA in character columns --------------------------------------------------
titanic %>%
  mutate_if(is.character, funs(ifelse(. == "", "NA", .))) -> titanic_1

titanic_1

#' Replace all NA values into "NA" in character columns --------------------------------------------------
titanic_1 %>% 
  mutate_if(is.character, funs(ifelse(is.na(.), "NA", .))) -> titanic_2

titanic_2

#' Replace all NA values into 0 in numeric columns --------------------------------------------------
titanic_2 %>% 
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .))) -> titanic_3

titanic_3

#' At once ---------------------------------------------------------------------
titanic %>%
  mutate_if(is.character, funs(ifelse(. == "", "NA", .))) %>%
  mutate_if(is.character, funs(ifelse(is.na(.), "NA", .))) %>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .))) -> titanic_replaced

titanic_replaced

#' Imputating 0 with mean or median value in body columns -----------------------------
titanic_replaced %>%
  mutate_if(is.numeric, funs(ifelse(.==0, mean(.), .)))

mean(titanic_replaced$body)
median(titanic_replaced$body)

median_value <- median(titanic$body, na.rm = T) 

titanic_replaced %>%
  mutate(body = ifelse(body==0, median_value, body))

titanic_replaced %>%
  mutate(body = ifelse(body==0, median_value, body)) -> titanic_imputed

titanic_imputed

#' Convert NA values into median values --------------------------------------------------------------------------
titanic %>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), median(., na.rm=T), .))) -> t_tmp; # summary(t_tmp)

head(t_tmp)

# '_imp' suffix imputation --------------------------------------------------------------------------
titanic %>%
  mutate_if(is.numeric, funs(imp=ifelse(is.na(.), median(., na.rm=T), .))) -> t_tmp; # summary(t_tmp) # notice the "imp="

head(t_tmp)
# str(t_tmp)

#' Eliminate all data which has at least a NA --------------------------------------------------------------------------
na.omit(titanic) 

#' Convert data classes --------------------------------------------------------------------------
titanic_imputed$pclass <- as.factor(titanic_imputed$pclass)
# titanic$ticket <- as.character(titanic$ticket)
titanic_imputed$survived <- factor(titanic_imputed$survived, levels=c(0,1), labels=c('dead', 'survived'))
glimpse(titanic_imputed)

#' Check the data class --------------------------------------------------------------------------
class(titanic$embarked) # characeter
levels(titanic$embarked) # NULL
table(titanic$embarked)

titanic$embarked <- as.factor(titanic$embarked)
table(titanic$embarked)
table(titanic$embarked, useNA='always')

titanic$cabin <- as.factor(titanic$cabin)
table(titanic$cabin)
cabin <- as.data.frame(table(titanic$cabin)); names(cabin) <- c('room', 'count')

titanic %>%
  mutate(cabin = as.character(cabin)) %>%
  select(cabin) %>%
  group_by(cabin) %>%
  tally() %>%
  arrange(desc(n)) %>%
  filter(cabin != "") -> df.cabin

df.cabin %>% 
  ggplot(aes(cabin, n)) + 
  geom_bar(stat = "identity") +
  theme(legend.position = 'none', axis.text.x = element_text(angle = 90))

#' Imdb Dataset --------------------------------------------------------------------------
#' 
#' NA and outlier --------------------------------------------------------------------------
df_imdb <- read_csv('./data/imdb-5000-movie-dataset.zip')
# summary(df_imdb)

#' dplyr::drop_na() --------------------------------------------------------------------------
sum(is.na(df_imdb$gross)) # how many NAs in a certain column...
df_imdb$gross[df_imdb$gross < 0] <- NA # replace data which is below 0 to NA...

# summary(df_imdb$budget) 

df_imdb %>% nrow()

df_imdb %>%
  drop_na(budget) -> df_imdb_budget_na_drop ; # summary(df_imdb_budget_na_drop); nrow(df_imdb_budget_na_drop)

boxplot(df_imdb_budget_na_drop$budget, horizontal = T) # need to remove outliers of budget columns...
boxplot(df_imdb_budget_na_drop$budget)$stat 
# [1,] 2.18e+02
# [2,] 6.00e+06
# [3,] 2.00e+07
# [4,] 4.40e+07
# [5,] 1.00e+08

#' Budget < 2.18e+02 or budget > 1.00e+08 : outliers...
df_imdb_budget_na_drop %>%
  filter(budget >= 2.18e+02 & budget <= 1.00e+08) -> df_imdb_budget_na_oulier_drop

boxplot(df_imdb_budget_na_oulier_drop$budget, horizontal = T)

df_imdb_budget_na_oulier_drop %>%
  ggplot(aes("", budget)) + 
  geom_boxplot(notch = T, fill = "gray") +
  coord_flip() +
  geom_jitter(aes(colour = factor(country), alpha =.05)) +
  theme(legend.position = "none")

df_imdb_budget_na_oulier_drop %>% 
  ggplot(aes(language, budget, col = language)) + 
  geom_boxplot() + 
  theme(legend.position = 'none', axis.text.x = element_text(angle = 90))

nrow(df_imdb)
nrow(df_imdb_budget_na_oulier_drop)

#' Boxplot Whisker And Oulier Fence Mean ------------------------------------------------------
boxplot(df_imdb_budget_na_drop$budget)
IQR(df_imdb_budget_na_drop$budget)
1.5*IQR(df_imdb_budget_na_drop$budget)

fivenum(df_imdb_budget_na_drop$budget)

#' Q3
fivenum(df_imdb_budget_na_drop$budget)[4]

#' Q3 + 1.5*IQR :: Outlier Upper Fence...
fivenum(df_imdb_budget_na_drop$budget)[4] + 1.5*IQR(df_imdb_budget_na_drop$budget) # 103500000
boxplot(df_imdb_budget_na_drop$budget)$stat[5] # stat[5] are outlier is upper fence

#' Upper Whisker...
df_imdb_budget_na_drop %>%
  filter(budget > fivenum(df_imdb_budget_na_drop$budget)[4]) %>%
  filter(budget < fivenum(df_imdb_budget_na_drop$budget)[4] + 1.5*IQR(df_imdb_budget_na_drop$budget)) %>%
  select(budget) %>% pull() %>% max()

df_imdb_budget_na_drop %>%
  filter(budget == 1.03e+08) # %>% View() # three movies...

df_imdb_budget_na_drop %>%
  filter(budget > 1.03e+08) %>% select(budget) %>% pull() %>% range()
  
#' Data Sampling :: sample_n(), sample_n(data, replace=T), sample_frac()...
df_imdb %>% sample_n(10) # sampling without replacement...
df_imdb %>% sample_n(100, replace=T) # sampling with replacement...
df_imdb %>% sample_frac(0.01, replace=T) # sampling with replacement and fraction...

#' Selecting columns...
glimpse(df_imdb)
colnames(df_imdb)

#' 'color'~'movie_imdb_link' selecting...
df_imdb %>%
  select(color:movie_imdb_link)

#' Selecting columns whose name starts with certain characters...
df_imdb %>% select(starts_with('direc'))

#' Selecting columns whose name ends with certain characters....
df_imdb %>% select(ends_with('likes')) 

#' Start with 'actor' character and ends with 'likes' character...
df_imdb %>%
  select(starts_with('actor')) %>%
  select(ends_with('likes'))

#' Selecting columns whose name contains 'facebook' character...
df_imdb %>%
  select(contains('facebook'))

#' Eleminating certain columns with "-" sign...
df_imdb %>% 
  select(-director_name, -director_facebook_likes)

#' Selecting columns whhose name don't contain facebook' characters...
df_imdb %>%
  select(-contains('facebook'))

#' Extracting unique values in a certain variable...
nrow(df_imdb)

df_imdb %>%
  select(director_name) %>%
  distinct() 

#' How many unique values in a certain varible...
df_imdb %>%
  select(director_name) %>%
  distinct()  %>%
  nrow()

#' n(), n_distinct(), first(), last(), nth(x, n)
df_imdb %>% 
  select(director_name) %>%
  summarise(dict_count=n_distinct(director_name))

df_imdb %>%
  drop_na() %>%
  group_by(director_name) %>%
  tally() %>%
  arrange(desc(n))

head(df_imdb)
df_imdb %>%
  drop_na() %>%
  group_by(director_name) %>%
  mutate(first_duration = first(duration)) %>% 
  as.data.frame() %>%
  select(director_name, first_duration) %>% 
  head(10)

df_imdb %>%
  drop_na() %>%
  group_by(director_name) %>%
  mutate(last_duration = last(duration)) %>% 
  as.data.frame() %>%
  select(director_name, last_duration) %>% 
  head(10)

#' rename() --------------------------------------------------------------------
df_imdb %>% rename(direc_nm = director_name) # to be named(direc_nm) = variable name(director_name)

#' Make names() ----------------------------------------------------------------
#' 
#' Replace "_" with "."----
colnames(df_imdb)
make.names(names(df_imdb), unique=T)
names(df_imdb) <- tolower(gsub('_', '\\.', make.names(names(df_imdb), unique = T)))
colnames(df_imdb)

# melt / cast
data("airquality"); head(airquality)
names(airquality) <- tolower(names(airquality)); # head(airquality) 

aql <- melt(airquality, id.vars = c('month', 'day')) # head(aql)
aqw <- dcast(aql, month + day ~ variable) # head(aqw)

aql %>% as_tibble()
aqw %>% as_tibble()

airquality %>% 
  tbl_df() %>%
  gather(variable, value, -c(month, day)) -> aql; aql

aql %>%
  spread(variable, value) -> aqw; aqw

#' g_paid per cust...
tran <- read_csv('./data/transaction.csv')
tran %>% 
  group_by(custid, prod) %>%
  summarise(sum.amt = sum(amt)) -> cust_prod_amt_sum; head(cust_prod_amt_sum)

# pivotting...
names(cust_prod_amt_sum)
melted <- melt(cust_prod_amt_sum, id.vars=c('custid', 'prod'), measure.vars = c('sum.amt')); head(melted)
dcasted <- dcast(melted, custid ~ prod, value.var = 'value'); # head(dcasted)
sample_dcasted <- dcasted[1:2, ] 
sample_dcasted %>% as_tibble()

#' Define `id_spread_sum()` function ---------------------------------------------------
id_spread_sum <- function(df.grouped) {
  df.grouped %>%
    rowid_to_column(var = "id") %>%
    spread(eval(colnames(df.grouped[, 2])), eval(colnames(df.grouped[, 3])), fill = 0) %>%
    select(-id) %>%
    group_by(custid) %>%
    summarise_if(is.numeric, sum) -> df.result
  
  return(df.result)
}

#' Define `rowsum_ratio_df()` function -----------------------------------------
rowsum_ratio_df <- function(grouped_df, prefix) {
  grouped_df %>%
    id_spread_sum() %>%
    mutate(total = rowSums(select_if(., is.numeric))) %>%
    mutate_at(vars(-custid), funs(round(./total, 2))) %>%
    rename_at(vars(-custid), ~ paste0(prefix, .)) %>% select(-ncol(.)) -> df.result
  
  return(df.result)
}

cust_prod_amt_sum %>%
  id_spread_sum() 

cust_prod_amt_sum %>%
  rowsum_ratio_df(., "p.ratio_") # %>% View()

#' Replace all NA with 0 -------------------------------------------------------
dcasted %>% mutate_all(funs(ifelse(is.na(.), 0, .))) -> cust_prod_amt_sum

#' Make total sum column -------------------------------------------------------
cust_prod_amt_sum %>% mutate(total.amt = rowSums(.[-1])) -> cust_prod_amt_total_sum; 
cust_prod_amt_total_sum %>% as_tibble()

# head(cust_prod_amt_total_sum)
