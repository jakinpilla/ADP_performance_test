setwd("C:/Users/Daniel/ADP_performance_test")
getwd()

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
# TDM inspect----
# TDM 일부내용 확인하여 보기(inspect())----
inspect(tdm_contents[1:10, 1:10])

# TDM에서 빈출어휘 살펴보기(findAssocs())----
findAssocs(tdm_contents, "1박2일", .2)
findAssocs(tdm_contents, "복면가왕", .2)

tdm_contents_mat <- as.matrix(tdm_contents)
tdm_rowsum <- rowSums(tdm_contents_mat)
tdm_rowsum_desc <- tdm_rowsum[order(tdm_rowsum, decreasing=T)]; tdm_rowsum_desc

#  TDM에서 바로 합을 구하기
library(slam)
as.data.frame(row_sums(tdm_contents, na.rm=T)) -> wd_count_df; head(wd_count_df)
data.frame(words = rownames(wd_count_df), freq = wd_count_df$`row_sums(tdm_contents, na.rm = T)`) -> wd_count_df
wd_count_df %>%
  colnames()
wd_count_df %>%
  arrange(desc(freq)) -> wd_count_df ; wd_count_df %>% head(100)

# 문서 유사도 구하기----
as.matrix(tdm_contents) -> tdm_contents_mat; dim(tdm_contents_mat)
n <- nrow(tdm_contents_mat)
idx <- 1:n
sample_idx <- sample(idx, n * .2) # 20%만 문서 무작위 선택
sample_tdm_mat = tdm_contents_mat[sample_idx, ]
dim(sample_tdm_mat)

# norm_vec()----
norm_vec <- function(x) {
  return((x-min(x)) / (max(x) - min(x)))} # 정규화 함수 정의

# normalize----
tdm_sample_mat <- apply(sample_tdm_mat, 2, norm_vec)
sample_tdm_mat[1:10, 1:10]

# tdm_sample_mat 의 9번째 문서와 1~8번째 문서와의 유사도 계산
cord <- t(tdm_sample_mat[, 9]) %*% tdm_sample_mat[, 1:8]
cord

# 문서 유사도순으로 배열하기----
orders <- data.frame(docs = 'doc_9', scores = t(cord), stringsAsFactors = F); orders
orders %>%
  arrange(desc(scores))

# 문서 군집하기----
n <- nrow(tdm_contents_mat)
sample_idx <- sample(idx, n * .05)
tdm_sample_mat <- tdm_contents_mat[sample_idx, ]; dim(tdm_sample_mat)
tdm_sample_mat <- tdm_sample_mat[1:200, 1:100]; dim(tdm_sample_mat)# 계산 시간 다대하여 데이터를 축소함.

fit <- hclust(dist(t(tdm_sample_mat)), method='ward.D2')
plclust(fit)
rect.hclust(fit, k=5)

# matrix 변환, 행의 합, 빈도 정렬 및 wordcloud----

## matrix 변환----
as.matrix(tdm_contents) -> tdmat; dim(tdmat)

## 행의 합----
rowSums(tdmat) -> tdmat_sum

## 빈도 정렬
tdmat_sum[order(tdmat_sum, decreasing = T)] -> tdmat_arr; tdmat_arr[1:10]

## wordcloud----
as.data.frame(tdmat_arr) -> df; head(df)
windowsFonts()
windowsFonts(malgun=windowsFont("맑은 고딕"))
wordcloud(words = rownames(df), freq=df$tdmat_arr, 
          min.freq= 5, 
          max.words = 100,
          random.order = F,
          family = "malgun", 
          colors=brewer.pal(8, "Dark2"))









