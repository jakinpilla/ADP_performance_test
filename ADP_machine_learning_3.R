# PCA

Packages <- c('plyr', 'dplyr', 'tidyverse', 'data.table', 'reshape2', 'caret', 'rpart', 'GGally', 'ROCR', 'party', 
              'randomForest', 'dummies', 'curl', 'gridExtra')
lapply(Packages, library, character.only=T)


# install.packages('HSAUR')
library(HSAUR)
data('heptathlon')

head(heptathlon)
glimpse(heptathlon)
rownames(heptathlon)

# 큰 수가 좋은 점수가 되도록 최대값에서 빼준다.----
heptathlon$hurdles <- max(heptathlon$hurdles) - heptathlon$hurdles
heptathlon$run200m <- max(heptathlon$run200m) - heptathlon$run200m
heptathlon$run800m <- max(heptathlon$run800m) - heptathlon$run800m

cor(heptathlon)
ggpairs(heptathlon) # 독립변수간 높은 상관계수 확인(.7 이상) -> dimension reduction

# 정규화
scale(heptathlon)
cbind(as.data.frame(scale(iris[1:4])), iris$Species)

# 변수선택(모델링에 가장 적합한 변수)----
# install.packages('mlbench')
library(mlbench); data(Soybean)
glimpse(Soybean)

# 정보량이 0에 가까운 변수를 선택(분산이 0에 가까운...) caret :: nearZeroVar()
nearZeroVar(Soybean)
mySoybean <- Soybean[, -nearZeroVar(Soybean)] # 0에 가까운 분산 (정보량 적음)

# 변수간 상관관계가 높은 것을 삭제 caret :: findCorrelation() 
data("Vehicle"); head(Vehicle)

Vehicle %>%
  select(-Class) %>% 
  cor() %>%
  findCorrelation() -> high_cor; high_cor

Vehicle %>%
  select(-Class) %>% 
  cor() %>%
  findCorrelation(cutoff = .7) -> high_cor; high_cor # default = .9

Vehicle %>% 
  select(-high.cor) -> vehicle_filtered





























