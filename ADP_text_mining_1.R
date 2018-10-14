# install.packages('tm')
# install.packages('rJava')
# install.packages('KoNLP')
# install.packages('SnowballC')
# install.packages('slam')
library(rJava)
library(KoNLP); useSejongDic()
library(SnowballC)
library(slam)

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

tvpro_nm_df$tvpro_nm  = gsub(" ", "", tvpro_nm_df$tvpro_nm)
tvpro_nm_df

user_d <- tvpro_nm_df

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

# ko.words() 함수 만들기----
ko.words <- function(doc) {
  d <- as.character(doc)
  extractNoun(d)
}

























