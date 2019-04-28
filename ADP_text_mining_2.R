#' ---
#' title: "ADP TM_2"
#' author: "jakinpilla"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::github_document
#' ---

setwd("C:/Users/Daniel/ADP_performance_test")
getwd()

#' loading needed packages----
#' 
#+setup, include = FALSE
Packages <- c('plyr', 'dplyr', 'tidyverse', 'data.table', 'reshape2', 'caret', 
              'rpart', 'GGally', 'ROCR', 'randomForest', 'dummies', 'curl', 'gridExtra')
Packages_tm <- c('tm', 'rJava', 'KoNLP', 'SnowballC', 'slam', 'wordcloud')

lapply(Packages, library, character.only=T)
lapply(Packages_tm, library, character.only=T); useSejongDic()

#' loading text data -----------------------------------------------------------
tvpro <- read_delim("data/tvprograms.txt", "\t", escape_double = FALSE, trim_ws = TRUE)
head(tvpro$contents, 30)

#' Contents data ---------------------------------------------------------------
contents.vec <- tvpro$contents
contents.vec[is.na(contents.vec)] # contents에는 NA 값 없음
contents.vec %>% length()
head(contents.vec, 30)

#' Tagging and Nouns extraction ------------------------------------------------
#'

# contents_doc.vec <- NULL
# for (i in 1:length(contents.vec)) {
# 
#   contents.vec[i] %>% # tagging...
#     SimplePos09() %>%
#     unlist() %>%
#     unname() %>%
#     as.character() %>%
#     strsplit("\\+") %>%
#     unlist() -> ith.tagged
# 
#   ith.tagged[grep("/N", ith.tagged)] %>% # extract only Nouns...
#     paste(collapse = " ") -> ith.N_tagged
# 
#   contents_doc.vec <- c(contents_doc.vec, ith.N_tagged)
# }
# 
# contents_doc.vec %>%
#   enframe() %>%
#   as_tibble() -> df_contents_tagged
# 
# df_contents_tagged %>% 
#   rename(doc_num = name, 
#          content_tagged = value) %>%
#   write_csv("./data/df_contents_tagged.csv")

df_contents_tagged <- read_csv("./data/df_contents_tagged.csv")
df_contents_tagged %>% colnames()

#' 불용문자 제거 ---------------------------------------------------------------
#' 
df_contents_tagged$content_tagged <- gsub('[[:punct:]]+', "", df_contents_tagged$content_tagged)
df_contents_tagged$content_tagged <- gsub('[[:lower:]]+', "", df_contents_tagged$content_tagged)
df_contents_tagged$content_tagged <- gsub("n", "", df_contents_tagged$content_tagged)
df_contents_tagged$content_tagged <- gsub("N", "", df_contents_tagged$content_tagged)
df_contents_tagged$content_tagged <- gsub("■", "", df_contents_tagged$content_tagged)
df_contents_tagged$content_tagged <- gsub("△", "", df_contents_tagged$content_tagged)
df_contents_tagged$content_tagged <- gsub("<U+00A0>", "", df_contents_tagged$content_tagged)
df_contents_tagged$content_tagged <- gsub("▲", "", df_contents_tagged$content_tagged)
df_contents_tagged$content_tagged <- gsub("▷", "", df_contents_tagged$content_tagged)
df_contents_tagged$content_tagged <- gsub("▼", "", df_contents_tagged$content_tagged)
df_contents_tagged$content_tagged <- gsub("◇", "", df_contents_tagged$content_tagged)
df_contents_tagged$content_tagged <- gsub("◆", "", df_contents_tagged$content_tagged)
df_contents_tagged$content_tagged <- gsub("00a0", "", df_contents_tagged$content_tagged)
df_contents_tagged$content_tagged <- gsub("★", "", df_contents_tagged$content_tagged)

#' DTM 생성 --------------------------------------------------------------------
strsplit_space_tokenizer <- function(x) unlist(strsplit(as.character(x), " "))
df_contents_tagged$content_tagged[1] %>% strsplit_space_tokenizer()

options(mc.cores=1)
cps <- VCorpus(VectorSource(df_contents_tagged$content_tagged))

#+ include = FALSE
dtm_contents <- DocumentTermMatrix(cps,
                                   control = list(tokenize = strsplit_space_tokenizer,
                                                  removePunctuation=T,
                                                  wordLengths=c(2, 6),
                                                  weighting = weightTfIdf))

#' DTM inspect -----------------------------------------------------------------
tm::inspect(dtm_contents[1:50, ])
dtm_contents %>% colnames() -> terms.of_dtm

#' 연관어휘(findAssocs()) ------------------------------------------------------
findAssocs(dtm_contents, "1박2일", .2)
findAssocs(dtm_contents, "복면가왕", .2)

#' Word Count with slam package ------------------------------------------------
dtm_contents %>%
  slam::col_sums() %>%
  base::as.data.frame() %>%
  tibble::rownames_to_column() %>%
  as_tibble() %>%
  rename(words = rowname, freq = ".") %>%
  arrange(desc(freq))

#' 문서 유사도 구하기-----------------------------------------------------------
as.matrix(dtm_contents) -> dtm_contents_mat; dim(dtm_contents_mat)
n <- nrow(dtm_contents_mat)
idx <- 1:n

set.seed(2019)
sample_idx <- sample(idx, n * .2) # 20%만 문서 무작위 선택
sample_idx %>% length()
sample_dtm_mat = dtm_contents_mat[sample_idx, ]
dim(sample_dtm_mat)
sample_dtm_mat[1:10, ] %>% rownames()
sample_idx[1:10]


#' dtm_sample_mat 의 9번째 문서와 1~8번째 문서와의 유사도 계산 -----------------
#' 
#' dtm_sample_mat 의 9번째 문서 번호... ----------------------------------------
sample_idx[9] # 959 문서...
contents.vec[959]

cord <- sample_dtm_mat[9, ] %*% t(sample_dtm_mat)
cord[1:10]

#' 문서 유사도순으로 배열하기 --------------------------------------------------
orders <- data.frame(doc = 'doc_959', scores = t(cord), stringsAsFactors = F)
orders %>%
  as_tibble() %>%
  rownames_to_column() %>%
  rename(doc.num = rowname) %>%
  arrange(desc(scores)) -> similarity.for_doc_9th

similarity.for_doc_9th

contents.vec[as.numeric(similarity.for_doc_9th$doc.num[1:10])]

#' 문서 군집하기 ---------------------------------------------------------------
n <- nrow(dtm_contents_mat)
sample_idx <- sample(idx, n * .05)
dtm_sample_mat <- dtm_contents_mat[sample_idx, ]; dim(dtm_sample_mat)
dtm_sample_mat <- dtm_sample_mat[1:100, ]; dim(dtm_sample_mat) # 계산 시간 다대하여 데이터를 축소함.

# fit <- hclust(dist(dtm_sample_mat), method='ward.D2')
dtm_sample_mat %>%
  dist() %>%
  hclust(method = 'ward.D2') -> fit

plot(fit)
rect.hclust(fit, k=3)

#' Wordcount 및 Wordcloud ------------------------------------------------------
dtm_contents %>%
  slam::col_sums() %>%
  base::as.data.frame() %>%
  tibble::rownames_to_column() %>%
  as_tibble() %>%
  rename(words = rowname, freq = ".") %>%
  arrange(desc(freq)) -> wordcnt.data

wordcnt.data %>%
  slice(-grep("^\\d+$", words)) %>% # eliminate all rows which contains only digit number character
  slice(-3) %>% # to discard 00a0 things...
  slice(-31) %>%
  slice(-35) -> wordcnt.data_1
  
#' Wordcloud....
# install.packages("extrafont")
library(extrafont)
# font_import()

windowsFonts(hy=windowsFont("HYGothic-Extra"))
wordcloud(words = wordcnt.data_1$words, freq=wordcnt.data_1$freq,
          min.freq= 50, 
          max.words = 300,
          random.order = F,
          family = "hy", 
          colors=brewer.pal(8, "Dark2"))









