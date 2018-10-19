# loading needed packages----
Packages <- c('plyr', 'dplyr', 'tidyverse', 'data.table', 'reshape2', 'caret', 'rpart', 'GGally', 'ROCR', 'party', 
              'randomForest', 'dummies', 'curl', 'gridExtra')

Packages_tm <- c('tm', 'rJava', 'KoNLP', 'SnowballC', 'slam', 'wordcloud')

lapply(Packages, library, character.only=T)
lapply(Packages_tm, library, character.only=T); useSejongDic()

options(mc.cores=1) # not multi_core

# loading text data----
tvpro <- read_delim("data/tvprograms.txt", "\t", escape_double = FALSE, trim_ws = TRUE)
head(tvpro$contents, 10)

# Corpus----
cps <- VCorpus(VectorSource(tvpro$contents))
cps <- tm_map(cps, removePunctuation) # 구두점 제거
cps <- tm_map(cps, stripWhitespace) # 2개 이상 빈칸을 하나로
cps <- tm_map(cps, removeWords,
              c('mbs', 'kbs', 'sbs', '2016년도'))

# 명사 빈도 분석----
## ko.words() :: 명사 추출 함수 정의----
ko.words <- function(doc) {
  d <- as.character(doc)
  extractNoun(d)
}

## TermDocumentMatrix----
tdm_contents <- TermDocumentMatrix(cps, 
                                   control = list(tokenize = ko.words,
                                                  removePunctuation=T,
                                                  wordLengths=c(2, 6),
                                                  weighting = weightBin))
tdm_contents_mat <- as.matrix(tdm_contents)
tdm_rowsum <- rowSums(tdm_contents_mat)
tdm_rowsum_desc <- tdm_rowsum[order(tdm_rowsum, decreasing=T)]; tdm_rowsum_desc


library(slam)
as.data.frame(row_sums(tdm_contents, na.rm=T)) -> wd_count_df
wd_count_df %>%
  colnames()
wd_count_df %>% rename(freq = `row_sums(tdm_contents, na.rm = T)`) %>% head





