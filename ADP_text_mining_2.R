setwd("C:/Users/Daniel/ADP_performance_test")
getwd()

# loading needed packages----
Packages <- c('plyr', 'dplyr', 'tidyverse', 'data.table', 'reshape2', 'caret', 'rpart', 'GGally', 'ROCR', 'party', 'randomForest', 'dummies', 'curl', 'gridExtra')

Packages_tm <- c('tm', 'rJava', 'KoNLP', 'SnowballC', 'slam', 'wordcloud')

lapply(Packages, library, character.only=T)
lapply(Packages_tm, library, character.only=T); useSejongDic()

# loading text data----
tvpro <- read_delim("data/tvprograms.txt", "\t", escape_double = FALSE, trim_ws = TRUE)
head(tvpro$contents, 30)

# contents data----
contents_doc.vec <- tvpro$contents
contents_doc.vec[is.na(contents_doc.vec)] # contents에는 NA 값 없음
contents_doc.vec %>% length()
head(contents_doc.vec, 30)

# tagging and Nouns extraction ----
contents_doc.vec <- NULL
for (i in 1:length(contents.vec)) {
  
  contents.vec[i] %>% # tagging... 
    SimplePos09() %>% 
    unlist() %>% 
    unname() %>% 
    as.character() %>%
    strsplit("\\+") %>% 
    unlist() -> ith.tagged
  
  ith.tagged[grep("/N", ith.tagged)] %>% # extract only Nouns...
    paste(collapse = " ") -> ith.N_tagged
  
  contents_doc.vec <- c(contents_doc.vec, ith.N_tagged)
}

# 불용문자 제거 ----
contents_doc.vec <- gsub('[[:punct:]]+', "", contents_doc.vec)
contents_doc.vec <- gsub('[[:lower:]]+', "", contents_doc.vec)
contents_doc.vec <- gsub("n", "", contents_doc.vec)
contents_doc.vec <- gsub("N", "", contents_doc.vec)
contents_doc.vec <- gsub("■", "", contents_doc.vec)
contents_doc.vec <- gsub("△", "", contents_doc.vec)
contents_doc.vec <- gsub("<U+00A0>", "", contents_doc.vec)
contents_doc.vec <- gsub("▲", "", contents_doc.vec)
contents_doc.vec <- gsub("▷", "", contents_doc.vec)
contents_doc.vec <- gsub("▼", "", contents_doc.vec)
contents_doc.vec <- gsub("◇", "", contents_doc.vec)
contents_doc.vec <- gsub("◆", "", contents_doc.vec)
contents_doc.vec <- gsub("00a0", "", contents_doc.vec)
contents_doc.vec <- gsub("★", "", contents_doc.vec)

# DTM 생성----
strsplit_space_tokenizer <- function(x) unlist(strsplit(as.character(x), " "))
contents_doc.vec[1] %>% strsplit_space_tokenizer()

options(mc.cores=1)
cps <- VCorpus(VectorSource(contents_doc.vec))
dtm_contents <- DocumentTermMatrix(cps, 
                                   control = list(tokenize = strsplit_space_tokenizer,
                                                  removePunctuation=T,
                                                  wordLengths=c(2, 6),
                                                  weighting = weightTfIdf))

# DTM inspect----
tm::inspect(dtm_contents[1:50, ])
dtm_contents %>% colnames() -> terms.of_dtm

# 연관어휘(findAssocs())----
findAssocs(dtm_contents, "1박2일", .2)
findAssocs(dtm_contents, "복면가왕", .2)

# bigmemory for dtm ----

# install.packages('bigmemory')
# library(bigmemory)
# options(bigmemory.allow.dimnames = T)
# contents_big_mat <- as.big.matrix(as.matrix(dtm_contents)) 
# length(terms.of_dtm)
# dim(contents_big_mat)
# 
# colnames(contents_big_mat) <- terms.of_dtm
# 
# contents_big_mat[1, ] # encoding 문제 발생...극복방안은??
# 
# dtm_col_sum <- colSums(contents_big_mat)
# dtm_col_sum_desc <- dtm_col_sum[order(dtm_col_sum, decreasing=T)]; dtm_col_sum_desc[1:30]

#  DTM에서 slam 이용한 wordcount----
#  library(slam)

dtm_contents %>%
  slam::col_sums() %>%
  base::as.data.frame() %>%
  tibble::rownames_to_column() %>%
  as_tibble() %>%
  rename(words = rowname, freq = ".") %>%
  arrange(desc(freq))

# 문서 유사도 구하기----
as.matrix(dtm_contents) -> dtm_contents_mat; dim(dtm_contents_mat)
n <- nrow(dtm_contents_mat)
idx <- 1:n

set.seed(2019)
sample_idx <- sample(idx, n * .2) # 20%만 문서 무작위 선택
sample_dtm_mat = dtm_contents_mat[sample_idx, ]
dim(sample_dtm_mat)
sample_dtm_mat[1:10, ] %>% rownames()
sample_idx[1:10]


# dtm_sample_mat 의 9번째 문서와 1~8번째 문서와의 유사도 계산

# dtm_sample_mat 의 9번째 문서 번호...
sample_idx[9]

cord <- sample_dtm_mat[9, ] %*% t(sample_dtm_mat)
cord[1:10]

# 문서 유사도순으로 배열하기----
orders <- data.frame(docs = 'doc_959', scores = t(cord), stringsAsFactors = F)
orders %>%
  as_tibble() %>%
  rownames_to_column() %>%
  rename(doc.num = rowname) %>%
  arrange(desc(scores)) -> similarity.for_doc_959

similarity.for_doc_959

contents.vec[as.numeric(similarity.for_doc_959$doc.num[1:10])]

# 문서 군집하기----
n <- nrow(dtm_contents_mat)
sample_idx <- sample(idx, n * .05)
dtm_sample_mat <- dtm_contents_mat[sample_idx, ]; dim(dtm_sample_mat)
dtm_sample_mat <- dtm_sample_mat[1:200, ]; dim(dtm_sample_mat)# 계산 시간 다대하여 데이터를 축소함.

fit <- hclust(dist(dtm_sample_mat), method='ward.D2')
plot(fit)
rect.hclust(fit, k=5)

# wordcount 및 wordcloud----
dtm_contents %>%
  slam::col_sums() %>%
  base::as.data.frame() %>%
  tibble::rownames_to_column() %>%
  as_tibble() %>%
  rename(words = rowname, freq = ".") %>%
  arrange(desc(freq)) -> wordcnt.data

# wordcloud....
windowsFonts()
windowsFonts(malgun=windowsFont("맑은 고딕"))
wordcloud(words = wordcnt.data$words, freq=wordcnt.data$freq,
          min.freq= 50, 
          max.words = 300,
          random.order = F,
          family = "malgun", 
          colors=brewer.pal(8, "Dark2"))









