install.packages('tm')
install.packages('rJava')
install.packages('KoNLP')
install.packages('SnowballC')
install.packages('slam')
library(rJava)
library(KoNLP); useSejongDic()
library(SnowballC)
library(slam)

Packages <- c('tidyverse', 'data.table', 'reshape2', 'caret', 'rpart', 'GGally', 'ROCR', 'party', 
              'randomForest')

lapply(Packages, library, character.only=T)

options(mc.cores=1) # not multi_core

# 텍스트마이닝 일반
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


rm(list = ls())
gc()
