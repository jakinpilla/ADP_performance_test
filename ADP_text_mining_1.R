#' ---
#' title: "ADP TM_1"
#' author: "jakinpilla"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::github_document
#' ---

setwd("/home/insa/ADP_performance_test")
getwd()

rm(list = ls()); gc()

# devtools::install_github('rstudio/rmarkdown')

#+ setup, include = FALSE
Packages <- c('plyr', 'dplyr', 'tidyverse', 'data.table', 'reshape2', 'caret', 'rpart', 'GGally', 'ROCR', 
              'party', 'randomForest', 'dummies', 'curl', 'gridExtra')
Packages_tm <- c('rJava', 'KoNLP', "tm", 'SnowballC', 'slam', 'wordcloud')

lapply(Packages, library, character.only=T)
lapply(Packages_tm, library, character.only=T); useSejongDic()

options(mc.cores=1) # not multi_core

useSejongDic();

#' 단어 사전에 추가하기 --------------------------------------------------------
tvpro_nm <- read_csv("./data/tvprograms_name.txt", col_names = FALSE); tvpro_nm
tvpro_nm_df <- as.data.frame(t(tvpro_nm))
rownames(tvpro_nm_df) <- NULL; tvpro_nm_df
rep('ncn', nrow(tvpro_nm_df))

tvpro_nm_df <- data.frame(tvpro_nm = tvpro_nm_df$V1, tag=rep('ncn', nrow(tvpro_nm_df)))
rownames(tvpro_nm_df) <- NULL
tvpro_nm_df

pro_nm <- as.character(tvpro_nm_df$tvpro_nm)
pro_nm_pasted <- gsub(" ", "_", pro_nm)
pro_nm_pasted

pro_nm_without_space <- gsub(" ", "", pro_nm)
pro_nm_without_space

tvpro_nm_union <- union(pro_nm_pasted, pro_nm_without_space)

user_d <- data.frame(tvpro_nm = tvpro_nm_union, "ncn" ) # using 'broadcasting'
user_d

#' Adding words into dic :: buildDictionary() ----------------------------------
dics <- c('sejong')
category <- 'TV 프로그램'
buildDictionary(ext_dic=dics, category_dic_nms = category, 
                user_dic = user_d, replace_usr_dic = F)


## mergeUserDic() ----
# mergeUserDic(data.frame("사이다", "ncn"))
# mergeUserDic(data.frame("한국은처음이지", "ncn"))
# mergeUserDic(data.frame(readLines("./data/000.txt"), "ncn"))

#' Data loadinng ---------------------------------------------------------------
tvpro <- read_delim("data/tvprograms.txt", "\t", escape_double = FALSE, trim_ws = TRUE)
head(tvpro, 10)
head(tvpro, 2)$date
head(tvpro, 2)$title
head(tvpro, 2)$contents

dim(tvpro)

#' Titie text preprocessing ----------------------------------------------------
head(tvpro$title, 10)

#' Remove punctuation  ---------------------------------------------------------
tvpro$title <- gsub("1박 2일", "1박_2일", tvpro$title)
tvpro$title <- gsub("아는 형님", "아는_형님", tvpro$title)
tvpro$title <- gsub("정글의 법칙", "정글의_법칙", tvpro$title)
tvpro$title <- gsub("발칙한 동거", "발칙한_동거", tvpro$title)

tvpro$contents <- gsub("1박 2일", "1박_2일", tvpro$contents)
tvpro$contents <- gsub("아는 형님", "아는_형님", tvpro$contents)

tvpro$contents <- gsub("정글의 법칙", "정글의_법칙", tvpro$contents)
tvpro$contents <- gsub("발칙한 동거", "발칙한_동거", tvpro$contents)

tvpro$title <- gsub('[[:punct:]]+', "", tvpro$title) #구두점 제거
tvpro$title <- gsub('2016년도', "", tvpro$title)  # 특정단어 제거
# tvpro$title <- gsub('\\d+', "", tvpro$title) # 숫자 제거 :: program_nm 내 숫자가 있으므로 여기서는 해선 안 됨
tvpro$title <- gsub('[ㄱ-ㅣ]', '', tvpro$title) # ㅋㅋㅋ, ㅜㅠ 등 제거
tvpro$title <- str_replace_all(tvpro$title, '[[:lower:]]', '') # 영어표현 모두 삭제

#' 영문, 한글 아닌 것 전부 제거 하지만 띄어쓰기까지 모두 제거됨 -----------------
# tvpro$title <- str_replace_all(tvpro$title, '[^[:alpha:]]', '') 

head(tvpro$title, 100)

#' Contents text preprocessing -------------------------------------------------
# tvpro$contents <- gsub('[[:punct:]]+', "", tvpro$contents)
tvpro$contents <- gsub('2016년도', "", tvpro$contents)  # 특정단어 제거
tvpro$contents <- gsub('▲', "", tvpro$contents)  # 특정단어 제거
tvpro$contents <- gsub('U00A0', "", tvpro$contents)  # 특정단어 제거
tvpro$contents <- gsub('U00A07', "", tvpro$contents)  # 특정단어 제거
# tvpro$contents <- gsub('\\d+', "", tvpro$contents) # 숫자 제거
tvpro$contents <- gsub('[ㄱ-ㅣ]', '', tvpro$contents) # ㅋㅋㅋ, ㅜㅠ 등 제거
tvpro$contents <- str_replace_all(tvpro$contents, '[[:lower:]]', '') # 영어 소문자 표현 모두 삭제

head(tvpro$contents, 10)

#' NA 값 제거----
title.vec <- tvpro$title
title.vec[is.na(title.vec)]
title.vec[is.na(title.vec)] <- 'dummy' # NA 있으면 추후 에러 발생, 제거 필ㅇ

contents.vec <- tvpro$contents
contents.vec[is.na(contents.vec)] # contents에는 NA 값 없음

# # ko.words() 함수 만들기
# ko.words <- function(doc) {
#   d <- as.character(doc)
#   extractNoun(d)
# }

#' SimplePos09() tagging -------------------------------------------------------
length(title.vec)
title.vec[1:10]

title.vec[1] %>% 
  SimplePos09() %>% 
  unlist() %>% 
  unname() %>% 
  strsplit("\\+") %>% 
  unlist() -> title.vec_1.tagged

#' Extracting Nouns ---------------------------------------------------------------
noun.vec_1 <- title.vec_1.tagged[grep("/N", title.vec_1.tagged)]; noun.vec_1

#' Extracting adjectives ----------------------------------------------------------
adj.vec_1 <- title.vec_1.tagged[grep("/P", title.vec_1.tagged)]; adj.vec_1

noun.vec_1 %>% paste(collapse = " ")

#' Sample title vec prac ----
sam.vec <- title.vec[1:2]
sam.vec_taged <- NULL
for (i in 1:length(sam.vec)) {
  sam.vec[i] %>% 
    SimplePos09() %>% 
    unlist() %>% 
    unname() %>% 
    strsplit("\\+") %>% 
    unlist() -> ith.tagged
  
  ith.tagged[grep("/N", ith.tagged)] %>%
    paste(collapse = " ") -> ith.N_tagged
  
  sam.vec_taged <- append(sam.vec_taged, ith.N_tagged)
}

sam.vec_taged

#' Tagging on Title.vec and Extracting Nouns ------------------------------------
# title_doc.vec <- NULL
# for (i in 1:length(title.vec)) {
#   
#   title.vec[i] %>% 
#     SimplePos09() %>% 
#     unlist() %>% 
#     unname() %>% 
#     strsplit("\\+") %>% 
#     unlist() -> ith.tagged
#   
#   ith.tagged[grep("/N", ith.tagged)] %>% # 형용사일 경우 "/P" 로 변경...
#     paste(collapse = " ") -> ith.N_tagged
#   
#   title_doc.vec <- c(title_doc.vec, ith.N_tagged)
# }
# 
# title_doc.vec %>% length()
# title_doc.vec <- gsub('[[:punct:]]+', "", title_doc.vec)
# # title_doc.vec <- gsub('[[:digit:]]+', "", title_doc.vec) # 1박 2일 때문에 안 됨.
# title_doc.vec <- gsub('[[:lower:]]+', "", title_doc.vec)
# title_doc.vec <- gsub("n", "", title_doc.vec)
# title_doc.vec <- gsub("N", "", title_doc.vec)
# 
# title_doc.vec %>%
#   enframe() %>%
#   rename(doc_num = name, 
#          title_tagged = value) %>%
#   write_csv("./data/df_title_tagged.csv")

df_title_tagged <- read_csv("./data/df_title_tagged.csv")
df_title_tagged %>% colnames()

#' 불용문자 제거 ---------------------------------------------------------------
#' 
df_title_tagged$title_tagged <- gsub('[[:punct:]]+', "", df_title_tagged$title_tagged)
df_title_tagged$title_tagged <- gsub('[[:lower:]]+', "", df_title_tagged$title_tagged)
df_title_tagged$title_tagged <- gsub("n", "", df_title_tagged$title_tagged)


#' DTM for Title vector data ---------------------------------------------------
strsplit_space_tokenizer <- function(x) unlist(strsplit(as.character(x), " "))
df_title_tagged$title_tagged[1] %>% strsplit_space_tokenizer()

options(mc.cores=1)
cps <- tm::VCorpus(tm::VectorSource(df_title_tagged$title_tagged))
dtm_title <- DocumentTermMatrix(cps, 
                                control = list(tokenize = strsplit_space_tokenizer,
                                               removePunctuation=T,
                                               wordLengths=c(2, 6),
                                               weighting = weightTf))
dtm_title
dtm_title_mat <- as.matrix(dtm_title)
dim(dtm_title_mat)


#' 만약 TDM이 너무 커서 as.matrix() 이후 메모리상에 할당되지 않을 때의 해결방법은??
#' 
#' Using `slam packaege` -------------------------------------------------------
dtm_title %>%
  slam::col_sums(na.rm = T) %>%
  as.data.frame() %>% 
  rownames_to_column() %>%
  as_tibble() %>%
  rename(words = rowname) %>%
  rename(freq = ".") ->  df.title_word_count
  
df.title_word_count # %>% View()

df.title_word_count$words <- gsub("★", "", df.title_word_count$words)
df.title_word_count %>%
  slice(-grep("^\\d+$", words)) -> df.title_word_count  # eliminate all rows which contains only digit number character

df.title_word_count$words[1:10] # %>% View()

#' 긍정, 부정 단어 사전, 카운트, 두 개의 집단으로  클러스터링, 시각화...
#' 
#' 영화 댓글, 점수 크롤링 후 데이터셋 만들기 / 형용사 추출 / 긍부정과 관련있는 형용사는 무엇
#' 영화의 긍부정 댓글 구분
#' 월별 긍부정 추이 확인
#'
#' tvpro_nm_union이 있는 행들만 추출 -------------------------------------------
# dimnames(dtm_title)
tvpro_nm_union
dtm_title_extracted <- dtm_title_mat[, dimnames(dtm_title)$Terms %in% tvpro_nm_union] # TDM 에서 추출한다.
dim(dtm_title_extracted) # 9개의 행들만 추출된 matrix
dtm_title_extracted %>% colnames()

title_mat <- dtm_title_extracted
str(title_mat)
colnames(title_mat)

#' Title data만 보았을때 각 프로그램 이름이 들어간 문건의 횟수 확인

title_mat %>%
  as.data.frame() %>%
  col_sums() %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  as_tibble() %>%
  rename(words = rowname) %>%
  rename(freq = ".")

dtm_title %>%
  slam::col_sums(na.rm = T) %>%
  as.data.frame() %>% 
  rownames_to_column() %>%
  as_tibble() %>%
  rename(words = rowname) %>%
  rename(freq = ".") ->  df.title_word_count

df.title_word_count %>%
  filter(words %in% tvpro_nm_union)

#' Contents data ---------------------------------------------------------------
contents.vec <- tvpro$contents
contents.vec[is.na(contents.vec)] # There is no NA in content vector...

# contents_doc.vec <- NULL
# for (i in 1:length(contents.vec)) {
#   
#   contents.vec[i] %>% 
#     SimplePos09() %>% 
#     unlist() %>% 
#     unname() %>% 
#     strsplit("\\+") %>% 
#     unlist() -> ith.tagged
#   
#   ith.tagged[grep("/N", ith.tagged)] %>%
#     paste(collapse = " ") -> ith.N_tagged
#   
#   contents_doc.vec <- c(contents_doc.vec, ith.N_tagged)
# }

# title_doc.vec %>%
#   enframe() %>%
#   rename(doc_num = name, 
#          title_tagged = value) %>%
#   write_csv("./data/df_title_tagged.csv")

# contents_doc.vec %>%
#   enframe() %>%
#   rename(doc_num = name,
#          contents_tagged = value) %>%
#   write_csv("./data/df_contents_tagged.csv")

df_contents_tagged <- read_csv("./data/df_contents_tagged.csv")
df_contents_tagged %>% colnames()

#' 불용문자 제거 ---------------------------------------------------------------
#' 
df_contents_tagged$contents_tagged <- gsub('[[:punct:]]+', "", df_contents_tagged$contents_tagged)
df_contents_tagged$contents_tagged <- gsub('[[:lower:]]+', "", df_contents_tagged$contents_tagged)
df_contents_tagged$contents_tagged <- gsub("n", "", df_contents_tagged$contents_tagged)
df_contents_tagged$contents_tagged <- gsub("N", "", df_contents_tagged$contents_tagged)

#' DTM 생성 --------------------------------------------------------------------
options(mc.cores=1)
cps <- tm::VCorpus(tm::VectorSource(df_contents_tagged$contents_tagged))
strsplit_space_tokenizer <- function(x) unlist(strsplit(as.character(x), " "))

dtm_contents <- DocumentTermMatrix(cps, 
                                   control = list(tokenize = strsplit_space_tokenizer,
                                                  removePunctuation=T,
                                                  wordLengths=c(2, 6),
                                                  weighting = weightTf))

dtm_contents
dtm_contents_mat <- as.matrix(dtm_contents) # 이렇게 하면 메모리 문제로 할당하지 못하는 경우가 많다.
dim(dtm_contents_mat)

#' tvpro_nm_union 이 있는 행들만 추출 -------------------------------------------
dtm_contents_extracted <- dtm_contents_mat[, dimnames(dtm_contents)$Terms %in% tvpro_nm_union]
contents_mat <- dtm_contents_extracted
str(contents_mat)
colnames(contents_mat)

#' instead ---------------------------------------------------------------------
inspect(dtm_contents[, dimnames(dtm_contents)$Terms %in% tvpro_nm_union])
dtm_contents[, dimnames(dtm_contents)$Terms %in% tvpro_nm_union] %>% 
  as.matrix() ->  dtm_contents_extracted # 이렇게 하면 필요한 열만 가지는 matrix를 메모리 할당 문제 없이 진행할 수 있다.

contents_mat <- dtm_contents_extracted

#' contents data만 보았을때 각 프로그램 이름이 들어간 문건의 횟수 확인 ---------

contents_mat %>%
  col_sums()

#' named vector를 date.frame으로 바꾸어서 그래프를 그리는 방법은? --------------
#'
#' sum matrix ------------------------------------------------------------------
total_mat <- title_mat + contents_mat

#' 날짜연산 --------------------------------------------------------------------
glimpse(tvpro)
class(tvpro$date)

#' 월별 집계를 위해 년도_월 변수 생성 -------------------------------------------
tvpro$month <- format(tvpro$date, '%Y-%m')
tvpro$month[1:5]

#' Add date and month columns----
head(as.data.frame(total_mat))
total_df <-  as.data.frame(total_mat)
dim(total_df)
date_df <- data.frame(date = tvpro$date, month=tvpro$month)
dim(date_df)

data <- cbind(date_df, total_df); head(data)

as.data.frame(total_mat) %>%
  as_tibble() %>%
  mutate(month = tvpro$month) -> data

#' 월별 단어 출현 횟수 ----------------------------------------------------------
head(data)
class(data)
colnames(data) <- c("day_night", "live_alone", "endless_challenge", "cohabitting",
                    "masked_singer", "three_meals", "brother_known",  "juggle_life",
                    "gimme_food", "month")

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
            sum.gimme_food = sum(gimme_food)) -> data_mon_sum; data_mon_sum

melted <- melt(data_mon_sum , id.vars = 'month'); head(melted)

data_mon_sum %>%
  gather(pro_nm, value, -month)

#' Visualization ---------------------------------------------------------------
data_mon_sum %>%
  gather(pro_nm, value, -month) %>%
  ggplot(aes(x = month, y = value, fill = pro_nm)) +
  geom_bar(position = 'dodge', stat = 'identity', color = 'black') +
  scale_y_continuous()


data_mon_sum %>%
  gather(pro_nm, value, -month) %>%
  ggplot(aes(x = month, y = value, fill = pro_nm)) +
  geom_bar(position = 'dodge', stat = 'identity', color = 'black') +
  scale_y_continuous(labels = scales::percent)


#' Just guess what tv programes are with frequent words... ---------------------
#' 
#' word count, word cloud 등

dtm_contents %>%
  slam::col_sums() %>%
  as.data.frame() %>%
  rownames_to_column() %>% 
  rename(freq = ".") %>%
  rename(words = rowname) %>%
  filter(freq >= 50) %>% 
  as_tibble() ->  data.for_wordcloud; data.for_wordcloud

#' text cleansing----
data.for_wordcloud %>%
  slice(-grep("^\\d+$", words)) -> data.for_wordcloud  # eliminate all rows which contains only digit number character

data.for_wordcloud  # %>% View()

#' Wordcloud----
windowsFonts(hy=windowsFont("HYGothic-Extra"))

wordcloud(words = data.for_wordcloud$words, 
          freq=(data.for_wordcloud$freq/min(data.for_wordcloud$freq)), 
          random.order=FALSE, 
          colors=brewer.pal(6,"Dark2"), 
          random.color=TRUE,
          family = 'hy')

#' 워드크라우드를 보고 프로그램명 유추 :: 무한도전, 복면가왕, 1박2일, 삼시세끼, 한끼줍쇼, 나혼자산다, 아는형님 등으로 유추가능
