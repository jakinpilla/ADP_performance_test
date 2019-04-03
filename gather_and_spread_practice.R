# gather practice with mini stocks data
# https://tidyr.tidyverse.org/reference/gather.html

df <- data_frame(V1 = c(2, 8, 1),
                 V2 = c(7, 3, 5),
                 V3 = c(9, 6, 5))
df

df %>%
  rownames_to_column('id') %>%
  gather(dept, cnt, V1:V3)

stocks <- tibble(
  time = as.Date('2009-01-01') + 0:9,
  X = rnorm(10, 0, 1),
  Y = rnorm(10, 0, 2),
  Z = rnorm(10, 0, 4)
)

stocks

gather(stocks, "stock", "price", -time)
stocks %>% gather("stock", "price", -time)

library(dplyr)
mini_iris <-
  iris %>%
  group_by(Species) %>%
  slice(1)
mini_iris %>% gather(key = "flower_att", value = "measurement", -Species)