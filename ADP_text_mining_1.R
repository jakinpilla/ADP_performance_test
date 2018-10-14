# install.packages('tm')
# install.packages('rJava')
# install.packages('KoNLP')
# install.packages('SnowballC')
# install.packages('slam')
library(rJava)
library(KoNLP); useSejongDic()
library(SnowballC)
library(slam)
library(tm)

Packages <- c('tidyverse', 'data.table', 'reshape2', 'caret', 'rpart', 'GGally', 'ROCR', 'party', 
              'randomForest')

lapply(Packages, library, character.only=T)

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
head(tvpro, 2)

head(tvpro, 2)$date
head(tvpro, 2)$title
head(tvpro, 2)$contents

# remove punctuation 
tvpro$title <- gsub('[[:punct:]]+', "", tvpro$title)
tvpro$contents <- gsub('[[:punct:]]+', "", tvpro$contents)

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
tdm_title_extracted <- tdm_title[dimnames(tdm_title)$Terms %in% pro_names, ]
title_mat <- as.matrix(tdm_title_extracted)
str(title_mat)
rownames(title_mat)

## title data만 보았을때 각 프로그램 이름이 들어간 문건의 횟수 확인
### 나중에 for 문으로...
sum(title_mat[1, ]) # 537 :: 1박2일은 537건의 문건에서 등장(title만 보았을때)
sum(title_mat[2, ]) # 210 :: 나혼자산다 210번
sum(title_mat[3, ]) # 742 :: 무한도전 742번
sum(title_mat[4, ]) # 17 :: 발칙한 동거
sum(title_mat[5, ]) # 474 :: 복면가왕
sum(title_mat[6, ]) # 278 :: 삼시세끼
sum(title_mat[7, ]) # 105 :: 아는형님
sum(title_mat[8, ]) # 51 :: 정글의법칙
sum(title_mat[9, ]) # 363 :: 한끼줍쇼

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
### 나중에 for 문으로...
sum(contents_mat[1, ]) # 1767 :: 1박2일은 537건의 문건에서 등장(title만 보았을때)
sum(contents_mat[2, ]) # 349 :: 나혼자산다 210번
sum(contents_mat[3, ]) # 4417 :: 무한도전 742번
sum(contents_mat[4, ]) # 9 :: 발칙한 동거
sum(contents_mat[5, ]) # 1858 :: 복면가왕
sum(contents_mat[6, ]) # 1318 :: 삼시세끼
sum(contents_mat[7, ]) # 216 :: 아는형님
sum(contents_mat[8, ]) # 125 :: 정글의법칙
sum(contents_mat[9, ]) # 727 :: 한끼줍쇼

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

