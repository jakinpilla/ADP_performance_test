Packages <- c('plyr', 'dplyr', 'tidyverse', 'data.table', 'reshape2', 'caret', 'rpart', 'GGally', 'ROCR', 'party', 
              'randomForest', 'dummies', 'curl', 'gridExtra', 'rvest', 'httr', 'qgraph', 'xml2')
lapply(Packages, library, character.only=T)

# install.packages('qgraph')

library('rJava')
library('KoNLP'); useSejongDic()
library('tm')
library('stringr')

url_base <- 'http://movie.daum.net/moviedb/grade?movieId=99056&type=netizen&page='

all.reviews <- c() 
for(page in 1:100){
  
  url <- paste(url_base, page, sep='')
  
  read_html(url) %>%
    html_nodes("p.desc_review") %>%
    html_text() %>%
    str_trim() -> ith_reviews
  
  all.reviews <- c(all.reviews, ith_reviews)
}

all.reviews
all.reviews %>% length()

url_base <- 'http://movie.daum.net/moviedb/grade?movieId=99056&type=netizen&page='

all.scores <- c() 
for(page in 1:100){
  
  url <- paste(url_base, page, sep='')
  
  read_html(url) %>%
    html_nodes("em.emph_grade") %>%
    html_text() %>%
    as.numeric() -> ith_scores

  all.scores <- c(all.scores, ith_scores)
}

all.scores
all.scores %>% length()

tibble(comments = all.reviews,
       scores = all.scores) -> small.txt_data

small.txt_data %>%
  filter(nchar(comments) >=  3) -> small.txt_data

small.txt_data %>% View()

small.txt_data %>%
  write.csv('./data/movie_sing.review_score.csv', row.names = F, 
            fileEncoding = 'utf-8')

movie_sing_review_score <- read_csv("data/movie_sing.review_score.csv")
movie_sing_review_score %>%
  mutate(posi_neg = ifelse(scores >= 6, 1, 0)) -> movie_sing_review_score

table(movie_sing_review_score$posi_neg)

movie_sing_review_score %>%
  filter(scores == 6)

# 긍부정 단어사전 만들기 ----




