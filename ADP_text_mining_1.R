setwd("C:/Users/Daniel/ADP_performance_test")
getwd()

# install.packages('tm')
# install.packages('rJava')
# install.packages('KoNLP')
# install.packages('SnowballC')
# install.packages('slam')
# install.packages('wordcloud')

library(rJava)
library(KoNLP); useSejongDic()
library(SnowballC)
library(slam)
library(tm)
library(wordcloud)

Packages <- c('plyr', 'dplyr', 'tidyverse', 'data.table', 'reshape2', 'caret', 'rpart', 'GGally', 'ROCR', 'party', 
              'randomForest', 'dummies', 'curl', 'gridExtra')

Packages_tm <- c('rJava', 'KoNLP', 'SnowballC', 'slam', 'wordcloud')

lapply(Packages, library, character.only=T)
lapply(Packages_tm, library, character.only=T); useSejongDic()

options(mc.cores=1) # not multi_core


# 단어 사전에 추가하기----
tvpro_nm <- read_csv("./data/tvprograms_name.txt", col_names = FALSE)
tvpro_nm_df <- as.data.frame(t(tvpro_nm))
rownames(tvpro_nm_df) <- NULL; tvpro_nm_df
rep('ncn', nrow(tvpro_nm_df))

tvpro_nm_df <- data.frame(tvpro_nm = tvpro_nm_df$V1, tag=rep('ncn', nrow(tvpro_nm_df)))
rownames(tvpro_nm_df) <- NULL
tvpro_nm_df

pro_nm <- tvpro_nm_df$tvpro_nm
pro_nm_pasted <- gsub(" ", "", tvpro_nm_df$tvpro_nm)
pro_names <- union(pro_nm, pro_nm_pasted)
pro_names

user_d <- data.frame(tvpro_nm = pro_names, "ncn" ) # using 'broadcasting'

## adding words into dic :: buildDictionary()
dics <- c('sejong')
category <- 'TV 프로그램'
buildDictionary(ext_dic=dics, category_dic_nms = category, 
                user_dic = user_d, replace_usr_dic = F)


## mergeUserDic() ----
mergeUserDic(data.frame("사이다", "ncn"))
mergeUserDic(data.frame("한국은처음이지", "ncn"))
# mergeUserDic(data.frame(readLines("./data/000.txt"), "ncn"))

# data loadinng ----
tvpro <- read_delim("data/tvprograms.txt", "\t", escape_double = FALSE, trim_ws = TRUE)
head(tvpro, 10)
head(tvpro, 2)$date
head(tvpro, 2)$title
head(tvpro, 2)$contents

## titie text preprocessing----
head(tvpro$title, 10)
# remove punctuation 
tvpro$title <- gsub('[[:punct:]]+', "", tvpro$title) #구두점 제거
tvpro$title <- gsub('2016년도', "", tvpro$title)  # 특정단어 제거
# tvpro$title <- gsub('\\d+', "", tvpro$title) # 숫자 제거 :: program_nm 내 숫자가 있으므로 여기서는 해선 안 됨
tvpro$title <- gsub('[ㄱ-ㅣ]', '', tvpro$title) # ㅋㅋㅋ, ㅜㅠ 등 제거
tvpro$title <- str_replace_all(tvpro$title, '[[:lower:]]', '') # 영어표현 모두 삭제

# 영문, 한글 아닌 것 전부 제거 하지만 띄어쓰기까지 모두 제거됨----
# tvpro$title <- str_replace_all(tvpro$title, '[^[:alpha:]]', '') 

head(tvpro$title, 10)

## contents text preprocessing----
tvpro$contents <- gsub('[[:punct:]]+', "", tvpro$contents)
tvpro$contents <- gsub('2016년도', "", tvpro$contents)  # 특정단어 제거
tvpro$contents <- gsub('▲', "", tvpro$contents)  # 특정단어 제거
# tvpro$contents <- gsub('\\d+', "", tvpro$contents) # 숫자 제거
tvpro$contents <- gsub('[ㄱ-ㅣ]', '', tvpro$contents) # ㅋㅋㅋ, ㅜㅠ 등 제거
tvpro$contents <- str_replace_all(tvpro$contents, '[[:lower:]]', '') # 영어 소문자 표현 모두 삭제

head(tvpro$contents, 10)

## NA 값 제거----
title <- tvpro$title
title[is.na(title)] <- 'dummy' # NA 있으면 추후 에러 발생, 제거 필ㅇ
title[is.na(title)]

contents <- tvpro$contents
contents[is.na(contents)] # contents에는 NA 값 없음

# ko.words() 함수 만들기----
ko.words <- function(doc) {
  d <- as.character(doc)
  extractNoun(d)
}

# title data----

## TDM 생성----
options(mc.cores=1)
cps <- VCorpus(VectorSource(title))
tdm_title <- TermDocumentMatrix(cps, 
                                control = list(tokenize = ko.words,
                                               removePunctuation=T,
                                               wordLengths=c(2, 6),
                                               weighting = weightTf))
tdm_title
tdm_title_mat <- as.matrix(tdm_title)
dim(tdm_title_mat)

## pro_names가 있는 행들만 추출----
tdm_title_extracted <- tdm_title[dimnames(tdm_title)$Terms %in% pro_names, ] # TDM 에서 추출한다.
title_mat <- as.matrix(tdm_title_extracted)
str(title_mat)
rownames(title_mat)

## title data만 보았을때 각 프로그램 이름이 들어간 문건의 횟수 확인
for (i in 1:length(pro_nm_pasted)) {
  pro_freq = sum(title_mat[i, ])
  cat(pro_nm_pasted[i], ':', pro_freq, "\n", sep=" ")
}

# contents data----
## TDM 생성----
options(mc.cores=1)
cps <- VCorpus(VectorSource(contents))
tdm_contents <- TermDocumentMatrix(cps, 
                                   control = list(tokenize = ko.words,
                                                  removePunctuation=T,
                                                  wordLengths=c(2, 6),
                                                  weighting = weightTf))

tdm_contents
tdm_contents_mat <- as.matrix(tdm_contents)
dim(tdm_contents_mat)

## pro_names가 있는 행들만 추출----
tdm_contents_extracted <- tdm_contents[dimnames(tdm_contents)$Terms %in% pro_names, ]
contents_mat <- as.matrix(tdm_contents_extracted)
str(contents_mat)
rownames(contents_mat)

## contents data만 보았을때 각 프로그램 이름이 들어간 문건의 횟수 확인

length(pro_nm_pasted)
pro_nm_pasted[1]

for (i in 1:length(pro_nm_pasted)) {
  pro_freq = sum(contents_mat[i, ])
  cat(pro_nm_pasted[i], ':', pro_freq, "\n", sep=" ")
}

# sum matrix----
total_mat <- title_mat + contents_mat

# 날짜연산----
glimpse(tvpro)
class(tvpro$date)

## 월별 집계를 위해 년도_월 변수 생성----
tvpro$month <- format(tvpro$date, '%Y-%m')
tvpro$month[1:5]

# TDM -> DTM
total_mat <- t(total_mat)
dim(total_mat)

# add date and month columns----
head(as.data.frame(total_mat))
total_df <-  as.data.frame(total_mat)
dim(total_df)
date_df <- data.frame(date = tvpro$date, month=tvpro$month)
dim(date_df)

data <- cbind(date_df, total_df)

# aggregate date grouped by month
head(data)
class(data)
colnames(data) <- c("date", "month", "day_night", "live_alone", "endless_challenge", "cohabitting",
                    "masked_singer", "three_meals", "brother_known",  "juggle_life", "gimme_food" )

head(data)

data %>% 
  group_by(month) %>%
  summarise(sum.day_night = sum(day_night),
            sum.live_alone = sum(live_alone),
            sum.endless_challenge = sum(endless_challenge),
            sum.cohabitting = sum(cohabitting),
            sum.masked_singer =sum(masked_singer),
            sum.three_meals = sum(three_meals),
            sum.brother_known = sum(brother_known),
            sum.juggle_life = sum(juggle_life),
            sum.gimme_food = sum(gimme_food)) -> data_mon_sum; head(data_mon_sum)

melted <- melt(data_mon_sum , id.vars = 'month'); head(melted)

windows()
ggplot(melted, aes(x=month, y=value, fill=variable)) +
  geom_bar(position='dodge', stat='identity') + scale_y_continuous()

ggplot(melted, aes(x=month, y=value, fill=variable)) +
  geom_bar(position='fill', stat='identity') + scale_y_continuous(labels = scales :: percent)

# legend를 한글로 바꾸는 방법...

##-------------------------------------------------------------------------------------

# 그냥 contents 자주 나오는 단어를 보고 tv program 이름 유추해보기
# word count, word cloud 등

# word count----
# data loadinng ----
tvpro <- read_delim("data/tvprograms.txt", "\t", escape_double = FALSE, trim_ws = TRUE)
head(tvpro$contents, 10)

# text cleansing----
tvpro$contents <- gsub('[[:punct:]]+', "", tvpro$contents)
tvpro$contents <- gsub('2016년도', "", tvpro$contents)  # 특정단어 제거
tvpro$contents <- gsub('▲', "", tvpro$contents)  # 특정단어 제거
tvpro$contents <- gsub('△', "", tvpro$contents)  # 특정단어 제거

# tvpro$contents <- gsub('\\d+', "", tvpro$contents) # 숫자 제거
tvpro$contents <- gsub('[ㄱ-ㅣ]', '', tvpro$contents) # ㅋㅋㅋ, ㅜㅠ 등 제거

# 영어 소문자 표현 모두 삭제
# 잘 동작되지 않음
tvpro$contents <- str_replace_all(tvpro$contents, '[[:lower:]]', '') 
head(tvpro$contents, 10)

contents <- tvpro$contents
  
# 명사 빈도 분석----
## ko.words() :: 명사 추출 함수 정의----
ko.words <- function(doc) {
  d <- as.character(doc)
  extractNoun(d)
}

## TermDocumentMatrix----
options(mc.cores=1)
cps <- VCorpus(VectorSource(contents))
tdm_contents <- TermDocumentMatrix(cps, 
                                   control = list(tokenize = ko.words,
                                                  removePunctuation=T,
                                                  wordLengths=c(2, 6),
                                                  weighting = weightTf))

tdm_contents_mat <- as.matrix(tdm_contents)
dim(tdm_contents_mat)

length(rownames(tdm_contents_mat))
length(rowSums(tdm_contents_mat))
word_freq_df <- data.frame(words = rownames(tdm_contents_mat),
                           freq = rowSums(tdm_contents_mat))
rownames(word_freq_df) <- NULL
head(word_freq_df)

word_freq_df %>%
  arrange(desc(freq)) 

head(word_freq_df)
word_freq_df$words
nchar(word_freq_df$words)

word_freq_df %>%
  mutate(leng_words = str_length(words)) %>% # 각 단어의 character 수를 세기
  arrange(desc(freq)) %>%
  filter(leng_words > 2) %>%
  slice(1:100) -> word_df
  
# worocloud----
wordcloud(words = word_df$words, freq=word_df$freq, random.order=FALSE)

wordcloud(words = word_df$words, freq=(word_df$freq/min(word_df$freq)), random.order=FALSE, 
          colors=brewer.pal(6,"Dark2"), random.color=TRUE)
## 프로그램명 유추 :: 무한도전, 복면가왕, 1박2일, 삼시세끼, 한끼줍쇼, 나혼자산다, 아는형님 등으로 유추가능


# 월별 각 프로그램들의 등장 횟수 구하기
## TDM rowname이 유추된 프로그램명과 동일한 것만 추출
## extracted TDM -> DTM으로 변경
## DTM에 날짜 컬럼 붙이기(일자 및 월별 컬럼 두 개)
## 추출된 데이터를 월별로 집계로 -> 각 프로그램의 월별 등장 빈도수 파악
## 시각화(wordcloud)

## extracting pro_names rows from TDM
pro_names <- c('무한도전', '1박2일', '복면가왕', '삼시세끼', '한끼줍쇼', '나혼자산다', '아는형님')
tdm_contents_extracted <- tdm_contents[dimnames(tdm_contents)$Terms %in% pro_names, ]
class(tdm_contents_extracted)

contents_mat <- as.matrix(tdm_contents_extracted)
dim(contents_mat)
class(contents_mat)

## TDM matix -> DTM matix
dtm_contents_mat <- t(contents_mat)
dim(dtm_contents_mat)

# add date and month columns----
View(head(as.data.frame(dtm_contents_mat))[1:50, ])
dtm_df <-  as.data.frame(dtm_contents_mat)
dim(dtm_df)

tvpro$month <- format(tvpro$date, '%Y-%m')
date_df <- data.frame(date = tvpro$date, month=tvpro$month)
dim(date_df)
data <- cbind(date_df, dtm_df)
colnames(data)

glimpse(data)

## aggregating by month----
data %>% 
  group_by(month) %>%
  summarise(sum.day_night = sum(`1박2일`),
            sum.live_alone = sum(나혼자산다),
            sum.endless_challenge = sum(무한도전), 
            sum.mask_singer = sum(복면가왕),
            sum.three_meals = sum(삼시세끼),
            sum.brother_known = sum(아는형님),
            sum.gimme_food = sum(한끼줍쇼)) -> data_grouped

pro_words <- c('1박2일', '나혼자산다', '무한도전', '복면가왕', '삼시세끼', '아는형님', '한끼줍쇼')

# wordcloud for 2016-10 data----
melt(data_grouped, id.var='month') %>%
  filter(month == '2016-10') -> word_df

words_df <- cbind(word_df, pro_words)

wordcloud(words = words_df$pro_words, freq=words_df$value, random.order=FALSE)
wordcloud(words = words_df$pro_words, freq=(words_df$value/min(words_df$value)), 
          random.order=FALSE, colors=brewer.pal(6,"Dark2"), random.color=TRUE)

# ...

