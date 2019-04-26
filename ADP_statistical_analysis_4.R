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
ggplot(mtcars, aes(x=mpg)) + geom_histogram(binwidth=1)
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

# One-way ANOVA...
boxplot(weight ~ group, PlantGrowth,
        xlab = "group",
        ylab = "weight")

plant.aov <- aov(weight ~ group, PlantGrowth)
summary(plant.aov)

model.tables(plant.aov, type = "means")

plant.ph <- TukeyHSD(plant.aov)
plant.ph
plot(plant.ph)

# Two-way ANOVA with tw.csv dataset...
tw <- read_csv("./data/tw.csv")

boxplot(Satisfaction ~ Format*Subject, tw,
        xlab = "interaction",
        ylab = "satisfaction")

with(tw, 
     interaction.plot(Subject, Format, Satisfaction))

tw.aov <- aov(Satisfaction~ Format*Subject, tw)
summary(tw.aov)
model.tables(tw.aov, type = "means")

tw.format.ph <- TukeyHSD(tw.aov, which = "Format")
tw.format.ph
plot(tw.format.ph)

tw.subject.ph <- TukeyHSD(tw.aov, which = "Subject")
tw.subject.ph
plot(tw.subject.ph)

# Two-way ANOVA with pw.csv dataset...
pw <- read_csv("./data/pw.csv"); pw

boxplot(height ~ plant*water, pw,
        xlab = "interaction",
        ylab = "height")

with(pw, 
     interaction.plot(water, plant, height))

pw.aov <- aov(height ~ plant*water, pw)
summary(pw.aov)
model.tables(pw.aov, type ="means")

pw.ph <- TukeyHSD(pw.aov, which = "plant:water")
pw.ph

op <- par(mar = c(5, 8, 4, 2))
plot(pw.ph, cex.axis = .7, las = 1)
par(op)

# Regression Analysis...
ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point(aes(shape = Species), size = 1.5) +
  geom_smooth(method = "lm") +
  xlab("Sepal Length") + ylab("Sepal Width")

# ANCOVA model...
sepals.lm <- lm(Sepal.Width ~ Sepal.Length*Species, data = iris)
anova(sepals.lm)
summary(sepals.lm)

# ANCOVA model...
iris2 = subset(iris, Species != "setosa", drop = T)
sepal2.lm <- lm(Sepal.Width ~ Sepal.Length*Species, data = iris2)
anova(sepal2.lm)

# ANCOVA model...
sepal3.lm <- lm(Sepal.Width ~ Sepal.Length + Species, data = iris2)
anova(sepal3.lm)

# ANCOVA model...
sepal4.lm <- lm(Sepal.Width ~ Sepal.Length, data = iris2)
summary(sepal4.lm)

ggplot(data = iris2, aes(x=Sepal.Length, y=Sepal.Width)) +
  geom_point(aes(shape = Species, color = Species), size = 1.5) +
  geom_smooth(method = "lm") +
  xlab("Sepal Length") + ylab("Sepal Width")

# ANCOVA model...
sepal5.lm <- lm(Sepal.Width ~ Sepal.Length + Species, data = iris)
anova(sepal5.lm)

sepals.full <- lm(Sepal.Width ~ Sepal.Length + Species, data = iris)

sepal.rest <- lm(Sepal.Width ~ Sepal.Length, data = iris)

anova(sepals.full, sepal.rest)

# Regression Analysis...
powerplant <- read_csv("./data/powerplant.csv")
lm(Output ~ Pressure + Temp, powerplant)
lm(Output ~ Pressure*Temp, powerplant)

lm.fit <- lm(Volume ~ Girth, trees)
lm.fit
summary(lm.fit)
anova(lm.fit)

poly.fit <- lm(Volume ~ Girth + I(Girth^2), trees)
anova(lm.fit, poly.fit)

lm.fit <- lm(Volume ~ Girth, trees)
coef(lm.fit)
confint(lm.fit)

plot(Volume ~ Girth, trees,
     main = "Scatter plot with polynomial curve")
abline(lm.fit, col = "red")
lines(lowess(trees$Girth, trees$Volume), col = "blue")

poly.fit <- lm(Volume ~ Girth + I(Girth^2), trees)
b <- coef(poly.fit)

plot(Volume ~ Girth, trees,
     main = "Scatter plot with polynomial curve")
curve(b[1] + b[2]*x + b[3]*x^2, col = "red", add = T)
lines(lowess(trees$Girth, trees$Volume), col = "blue")

lm.fit <- lm(Volume ~ Girth, trees)
newtrees <- data.frame(Girth = c(17.2, 12.0, 11.4))
predict(lm.fit, newtrees, interval = "prediction")

head(faithful)
ggplot(faithful, aes(x = waiting, y = eruptions)) +
  geom_point() + geom_smooth(method = "lm")

eruption.lm <- lm(eruptions ~ waiting, data = faithful)
summary(eruption.lm)

anova(eruption.lm)

coeffs <- coefficients(eruption.lm)
coeffs

waiting <- 80

duration <- coeffs[1] + coeffs[2] * waiting
duration

newdata <- data.frame(waiting = 80)
predict(eruption.lm, newdata)
predict(eruption.lm, newdata, interval = "confidence")

head(stackloss)
ggpairs(stackloss)

stackloss.lm <- lm(stack.loss ~ Air.Flow + Water.Temp + Acid.Conc., 
                   data = stackloss)
summary(stackloss.lm)

stackloss.rlm <- lm(stack.loss ~ Air.Flow + Water.Temp, 
                   data = stackloss)
summary(stackloss.rlm)

anova(stackloss.lm, stackloss.rlm)

newdata <- data.frame(Air.Flow = 72, Water.Temp = 20)
predict(stackloss.rlm, newdata)
predict(stackloss.rlm, newdata, interval = "confidence")
predict(stackloss.rlm, newdata, interval = "prediction")


# Model Diagnostics -------------------------------------------------------

lm.fit <- lm(Volume ~ Girth, trees)
resids <- rstandard(lm.fit)
resid(lm.fit)
rstudent(lm.fit)

shapiro.test(resids)
plot(lm.fit, which = 2)

plot(lm.fit, which = 1)
plot(lm.fit, which = 3)

plot(lm.fit, which = 4)
abline(h = 4/(31-1+1), col = "red")

plot(lm.fit, which = 5)
abline(v=2*(1+1)/31, col = "blue")

plot(lm.fit, which = 6)


# Multicollinearity -------------------------------------------------------

mtcars %>%
  select(mpg, disp, hp, wt, drat) %>%
  ggpairs()

fit <- lm(mpg ~ disp + hp + wt + drat, data = mtcars)

summary(fit)

anova(fit)

# install.packages("car")
library(car)

vif(fit)


# The "Best" Regression Model with state.x77 dataset -------------------------------------------------------

state.x77 %>% 
  as_tibble() %>%
  select(Population, Income, Illiteracy, Frost, Murder) %>% GGally::ggpairs()

fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data = state.x77)

summary(fit)

fit1 <- lm(Murder ~ Population + Illiteracy, data = state.x77)

fit2 <- lm(Murder ~ Population + Illiteracy + Income + Frost, data = state.x77)

anova(fit1, fit2)

AIC(fit1, fit2)

fit1 <- lm(Murder ~ 1, data = state.x77)
fit2 <- lm(Murder ~ Population + Illiteracy + Income + Frost, data = state.x77)

library(MASS)
stepAIC(fit2, direction = "backward")
stepAIC(fit1, direction = "forward", 
        scope = list(lower = fit1, upper = fit2))
stepAIC(fit1, direction = "both")

# install.packages("leaps")
library(leaps)

leaps <- regsubsets(Murder ~ Population + Illiteracy + Income + Frost, data = state.x77, nbest = 4)

plot(leaps, scale = "adjr2")
plot(leaps, scale = "bic")


# Standardized Regression Coefficients -------------------------------------

fit1 <- lm(Murder ~ Population + Illiteracy, data = state.x77)
fit2 <- lm(scale(Murder) ~ scale(Population) + scale(Illiteracy), data = state.x77)

summary(fit2)

# install.packages("QuantPsyc")
library(QuantPsyc)
lm.beta(fit1)


# Ridge Regression --------------------------------------------------------

library(glmnet)

y <- mtcars$hp
x <- mtcars %>%
  select(mpg, wt, drat) %>%
  data.matrix()






























