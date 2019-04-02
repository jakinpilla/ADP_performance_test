Packages <- c('plyr', 'dplyr', 'tidyverse', 'data.table', 'reshape2', 'caret', 'rpart', 'GGally', 'ROCR', 'party', 
              'randomForest', 'dummies', 'curl', 'gridExtra', 'rvest', 'httr', 'qgraph', 'xml2')
lapply(Packages, library, character.only=T)

# install.packages('qgraph')

library('rJava')
library('KoNLP'); useSejongDic()
library('tm')
library('stringr')


url_base <- 'http://movie.daum.net/moviedb/grade?movieId=99056&type=netizen&page='   # 크롤링 대상 URL



all.reviews <- c() 

for(page in 1:500){    ## 500페이지 까지만 수집 (본인이 나름대로 설정하면 됨) 
  
  url <- paste(url_base, page, sep='')   #url_base의 뒤에 페이지를 1~500 까지 늘리면서 접근
  
  htxt <- read_html(url)                       # html 코드 불러오기
  
  comments <- html_nodes(htxt, 'div') %>% html_nodes('p')  ## comment 가 있는 위치 찾아 들어가기 
  
  reviews <- html_text(comments)               # 실제 리뷰의 text 파일만 추출
  
  reviews <- repair_encoding(reviews, from = 'utf-8')  ## 인코딩 변경
  
  if( length(reviews) == 0 ){ break }                              #리뷰가 없는 내용은 제거
  
  reviews <- str_trim(reviews)                                      # 앞뒤 공백문자 제거
  
  all.reviews <- c(all.reviews, reviews)                          #결과값 저장
  
}

##불필요 내용 필터링

all.reviews <- all.reviews[!str_detect(all.reviews,"평점")]   # 수집에 불필요한 단어가 포함된 내용 제거
all.reviews[1]
all.reviews[9]
all.reviews[11]

all.reviews %>% length()
all.reviews[317]


?html_nodes
url <- paste(url_base, 1, sep='')
htxt <- read_html(url)
html_nodes(htxt, 'em')

html_nodes(htxt, 'em') %>% html_text() -> id_score.vec

id_score.vec %>% length()

id_score <- NULL
for(i in 1:length(id_score.vec)) {
  if(i %% 2 == 0) {
    id_score <- c(id_score, id_score.vec[i])
  }
}


?html_children

all.id_scores <- c() 

for(page in 1:500){    ## 500페이지 까지만 수집 (본인이 나름대로 설정하면 됨) 
  
  url <- paste(url_base, page, sep='')   #url_base의 뒤에 페이지를 1~500 까지 늘리면서 접근
  
  htxt <- read_html(url)                       # html 코드 불러오기
  
  id_scores <- html_nodes(htxt, 'em') %>% html_text()  ## comment 가 있는 위치 찾아 들어가기 
  
  all.id_scores <- c(all.id_scores, id_scores)                          #결과값 저장
  
}

all.id_scores 
id_score <- NULL
for(i in 1:length(all.id_scores)) {
  if(i %% 21 != 0) {
    id_score <- c(id_score, all.id_scores[i])
  }
}

id_score

score <- NULL
for(i in 1:length(id_score)) {
  if(i %% 2 == 0) {
    score <- c(score, id_score[i])
  }
}

score
length(score)

data.frame(review = all.reviews, score = score[1:317]) %>% View()
