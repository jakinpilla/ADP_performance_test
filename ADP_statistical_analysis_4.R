# install.packages("tidyverse")
library(tidyverse)

# install.packages("nycflights13")
library(nycflights13)

flights

##

filter(flights, month == 1, day == 1)
flights[flights$month == 1 & flights$day == 1, ]

##

arrange(flights, year, month, day)
arrange(flights, desc(arr_delay))

##

select(flights, year, month, day)
select(flights, year:day)
select(flights, -(year:day))

select(flights, starts_with("dep"))
select(flights, ends_with("delay"))
select(flights, contains("time"))
# select(flights, matches("^dep"))

select(flights, tail_num = tailnum)
rename(flights, tail_num = tailnum)

##

mutate(flights,
       gain = arr_delay - dep_delay,
       speed = distance / air_time * 60
) # %>% View()

transform(flights,
          gain = arr_delay - dep_delay,
          speed = distance / air_time * 60
) # %>% View()

mutate(flights,
       gain = arr_delay - dep_delay,
       gain_per_hour = gain / (air_time / 60)
)

transmute(flights,
          gain = arr_delay - dep_delay,
          gain_per_hour = gain / (air_time / 60)
)

##

summarise(flights,
          delay = mean(dep_delay, na.rm = TRUE)
)

##

sample_n(flights, 10)
sample_frac(flights, 0.01)

##

by_tailnum <- group_by(flights, tailnum)
delay <- summarise(by_tailnum,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE))
delay

by_dest <- group_by(flights, dest)
destinations <- summarise(by_dest,
                          planes = n_distinct(tailnum),
                          flights = n()
)
destinations

daily <- group_by(flights, year, month, day)
(per_day <- summarise(daily, flights = n()))
(per_month <- summarise(per_day, flights = sum(flights)))
(per_year <- summarise(per_month, flights = sum(flights)))

##

a1 <- group_by(flights, year, month, day)
a2 <- select(a1, arr_delay, dep_delay)
a3 <- summarise(a2,
                arr = mean(arr_delay, na.rm = TRUE),
                dep = mean(dep_delay, na.rm = TRUE))
a4 <- filter(a3, arr > 30 | dep > 30)

filter(
  summarise(
    select(
      group_by(flights, year, month, day),
      arr_delay, dep_delay
    ),
    arr = mean(arr_delay, na.rm = TRUE),
    dep = mean(dep_delay, na.rm = TRUE)
  ),
  arr > 30 | dep > 30
)

flights %>%
  group_by(year, month, day) %>%
  select(arr_delay, dep_delay) %>%
  summarise(
    arr = mean(arr_delay, na.rm = TRUE),
    dep = mean(dep_delay, na.rm = TRUE)
  ) %>%
  filter(arr > 30 | dep > 30)

# ==============================
# Creating a Scatter Plot
# ==============================

help(mtcars)
str(mtcars)

plot(mtcars$wt, mtcars$mpg)

plot(mtcars$wt, mtcars$mpg,
     main="Scatter plot with base graphics",
     xlab="wt", ylab="mpg")

###

library(ggplot2)
qplot(mtcars$wt, mtcars$mpg) # qplot: quick plot

qplot(wt, mpg, data=mtcars)
# This is equivalent to:
ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point() # aes: aesthetics

# ==============================
# Creating a Line Graph
# ==============================

help(pressure)
str(pressure)

plot(pressure$temperature, pressure$pressure, type="l")

plot(pressure$temperature, pressure$pressure, type="l",
     main="Line graph with base graphics",
     xlab="temperature", ylab="pressure")

plot(pressure$temperature, pressure$pressure, type="l")
points(pressure$temperature, pressure$pressure)

lines(pressure$temperature, pressure$pressure/2, col="red")
points(pressure$temperature, pressure$pressure/2, col="red")

###

library(ggplot2)
qplot(pressure$temperature, pressure$pressure, geom="line")

qplot(temperature, pressure, data=pressure, geom="line")
# This is equivalent to:
ggplot(pressure, aes(x=temperature, y=pressure)) + geom_line()

# Lines and points together
qplot(temperature, pressure, data=pressure, geom=c("line", "point"))
# This is equivalent to:
ggplot(pressure, aes(x=temperature, y=pressure)) + geom_line() + geom_point()

# ==============================
# Creating a Bar Graph
# ==============================

help(BOD)
str(BOD)

barplot(BOD$demand, names.arg=BOD$Time)

table(mtcars$cyl)
# There are 11 cases of the value 4, 7 cases of 6, and 14 cases of 8

# Generate a bar graph of counts
barplot(table(mtcars$cyl))

###

library(ggplot2)
ggplot(BOD, aes(x=factor(Time), y=demand)) + geom_bar(stat="identity")

###

qplot(mtcars$cyl)
# Treat cylas discrete
qplot(factor(mtcars$cyl))

qplot(factor(cyl), data=mtcars)
# This is equivalent to:
ggplot(mtcars, aes(x=factor(cyl))) + geom_bar()

# ==============================
# Creating a Histogram
# ==============================

hist(mtcars$mpg)
# Specify approximate number of bins with breaks
hist(mtcars$mpg, breaks=10)

###

library(ggplot2)
qplot(mtcars$mpg)

qplot(mpg, data=mtcars, binwidth=4)
# This is equivalent to:
ggplot(mtcars, aes(x=mpg)) + geom_histogram(binwidth=4)

# ==============================
# Creating a Box Plot
# ==============================

help(ToothGrowth)
str(ToothGrowth)

plot(ToothGrowth$supp, ToothGrowth$len)

# Formula syntax
boxplot(len~ supp, data=ToothGrowth)

# Put interaction of two variables on x-axis
boxplot(len~ supp+dose, data=ToothGrowth)

###
library(ggplot2)
qplot(ToothGrowth$supp, ToothGrowth$len, geom="boxplot")

qplot(supp, len, data=ToothGrowth, geom="boxplot")

# This is equivalent to:
ggplot(ToothGrowth, aes(x=supp, y=len)) + geom_boxplot()

qplot(interaction(supp, dose), len, data=ToothGrowth, geom="boxplot")

# This is equivalent to:
ggplot(ToothGrowth, aes(x=interaction(supp, dose), y=len)) + geom_boxplot()

# ==============================
# Plotting a Function Curve
# ==============================

curve(x^3-5*x, from=-4, to=4)

# Plot a user-defined function
myfun<-function(xvar) {
  1/(1+exp(-xvar+10))
}

curve(myfun(x), from=0, to=20)
# Add a line:
curve(1-myfun(x), add=TRUE, col="red")

###
library(ggplot2)
# This is equivalent to:
ggplot(data.frame(x=c(0, 20)), aes(x=x)) +
  stat_function(fun=myfun, geom="line")

# ==============================
# Correlation Analysis
# ==============================

cov(trees$Height, trees$Volume)
cov(trees)

cov(trees, use="pairwise")
# cov(trees, use="complete")

##

cor(trees$Height, trees$Volume)
# same as cor(trees$Height, trees$Volume, method="pearson")
cor(trees) # same as cor(trees, method="pearson")

cor(trees, use="pairwise")
# cor(trees, use="complete")

##

cor(trees$Height, trees$Volume, method="spearman")
cor(trees, method="spearman")

cor(trees, method="spearman", use="pairwise")
# cor(trees, method="spearman", use="complete")

##

cor.test(trees$Girth, trees$Volume)
# cor.test(trees$Girth, trees$Volume, method="pearson")

##

# install.packages("corrplot")
library(corrplot)

M <- cor(mtcars)
# head(round(M, 2))

# method = "circle""
corrplot(M, method = "circle")

# method = "color"
corrplot(M, method = "color")

# method = "number"
corrplot(M, method = "number")

##

# correlogram with hclust reordering
corrplot(M, order = "hclust")

# ==============================
# Analyss of Variance
# ==============================

# One-way ANOVA
boxplot(weight ~ group, PlantGrowth,
        xlab = "group",
        ylab = "weight")

plant.aov <- aov(weight ~ group, PlantGrowth)
summary(plant.aov)

model.tables(plant.aov, type = "means")

plant.ph <- TukeyHSD(plant.aov)
plant.ph
plot(plant.ph)

# Two-way ANOVA
tw <- read_csv("./data/tw.csv")

boxplot(Satisfaction ~ Format*Subject, tw,
        xlab = "interaction",
        ylab = "satisfaction")

with(tw, 
     interaction.plot(Subject, Format, Satisfaction))
