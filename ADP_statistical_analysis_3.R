# anova(analysis of variance)

# an analysis of variance(anova) allows you to compare the means of three or more independent samples.

# It is suitable when the values are drawn from a normal distribution and when the variance is
# approximately the same in each group.

# You can check the assumption of equal variance with a Barlett's test

# The null hypothesis for the test is that the mean for all groups is the same, and the alternative
# hypothesis is that the mean is different for at least one pair of groups

# anova takes advantage of the additivity property of variance, and we partition the variance into 
# treatment effect (real differences) and error (differences due to sampling errror or individual differences)

# the ratio of two variance follows the F distribution (named after Fisher)

## One-way ANOVA
## two-way ANOVA
## Post-hoc Tests

# One-way ANOVA
# we compare the means for three or more groups. Each group is defined by a different level of the factor.
# If the overall F test is significant, we are justified in conducting post-hoc tests to determine which
# pairs of means are significantly different

# Patition of variance

# treatment variance :: the differences among the group means. we compare each group mean to the overall
# average treating all the variances as a single group(between variation)

# error variance :: the differencesss among values within each group(within variation)

# the null hypothesis in one-way anova is that all the means are equal in the population. the alternative
# hypothesis is that at least one of the means is different from the others

setwd("C:/Users/Daniel/ADP_performance_test")
getwd()

# 여러 개의 패키지를 한 번에 읽기
# install.packages('car')

Packages <- c('plyr', 'dplyr', 'tidyverse', 'data.table', 'reshape2', 'caret', 'rpart', 'GGally', 'ROCR', 'party', 
              'randomForest', 'dummies', 'curl', 'gridExtra', 'car', 'MASS', 'leaps')

lapply(Packages, library, character.only=T)


glimpse(PlantGrowth); str(PlantGrowth)
PlantGrowth %>%
  select(group) %>%
  unique

boxplot(weight ~ group, PlantGrowth)

# one-way anova
plant.aov <- aov(weight ~ group, PlantGrowth)
summary(plant.aov)
anova(plant.aov)
model.tables(plant.aov, type='means')

# post-hoc test
plant.ph <- TukeyHSD(plant.aov); plant.ph
plot(plant.ph)

# Two-way ANOVA
# In two-way ANOVA, there are two factors. We will illustrate only the most basic version of two-way
# fixed effects ANOVA, which is a balanced factorial design.
# Let us call the factors A and B. If there are r levels of A and c levels of B, there will be rXc total
# groups, each of which will have the same number of data values.

# Partition of Variation
# We partition the total sums of squares into respective sums of squares.
# The two-way ANOVA is an effecient design because it allow us to conduct three hypothesis tests.
# The three null hypotheses are:
# 1. There is no main effect of A considered sepatately.
# 2. There is no main effect of B considered separately.
# 3. There is no interaction of A and B considered together.

tw <- read.csv('./data/tw.csv'); head(tw)
glimpse(tw)

tw %>%
  select(Format) %>%
  unique

tw %>%
  select(Subject) %>%
  unique

boxplot(Satisfaction ~ Format*Subject, tw)
with(tw, interaction.plot(Subject, Format, Satisfaction)) # 교호작용이 없음

tw.aov <- aov(Satisfaction ~ Format*Subject, tw)
summary(tw.aov)
model.tables(tw.aov, type = 'means')

# Post-hoc Test
tw.format.ph <- TukeyHSD(tw.aov, which = 'Format')
tw.format.ph
plot(tw.format.ph)

tw.subject.ph <- TukeyHSD(tw.aov, which = 'Subject')
tw.subject.ph
plot(tw.subject.ph)

# Two-way ANOVA (2)
pw <- read.csv('./data/pw.csv')
head(pw)

pw %>% 
  select(plant) %>%
  unique

pw %>% 
  select(water) %>%
  unique

boxplot(height ~ plant*water, pw)
with(pw, interaction.plot(water, plant, height)) # 교호작용이 있음

pw.aov <- aov(height ~ plant*water, pw)
summary(pw.aov)
model.tables(pw.aov, type='means')

# Post-hoc Test
pw.ph <- TukeyHSD(pw.aov, which= 'plant:water')
pw.ph
op <- par(mar=c(5, 8, 4, 2))
plot(pw.ph, cex.axis=.7, las=1)


# Regression Analysis----

# simple linear regression
head(trees)
glimpse(trees)
lm(Volume ~ Girth, trees)

# multiple linear regression
powerplant <- read.csv('./data/powerplant.csv'); glimpse(powerplant)
lm(Output ~ Pressure + Temp, powerplant)

# interaction terms
lm(Output ~ Pressure*Temp, powerplant)

# update model :: update()
# The update function allows you build a new model by adding or removing terms from an existing model.

# t-test ::: Significance test for model coefficients
## significance test for model coefficients tell you whether indivisual coefficient estimates are significantly differnt from 0.

# F-test :: Analysis of Variance :: ANOVA
## F-test tells you whether the model is significantly better at predicting compared with using the overalll mean value as a prediction.
## F-test can also be used to compare two models.
## the anova() function to perform an F_test to compare a more complex model to simpler model.


lm.fit <- lm(Volume ~ Girth, trees)
summary(lm.fit)
anova(lm.fit)

poly.fit <- lm(Volume ~ Girth + I(Girth^2), trees)
anova(lm.fit, poly.fit)

coef(lm.fit)
confint(lm.fit)
plot(trees$Volume, trees$Girth)
plot(Girth ~ Volume, trees) # same as plot(trees$Volume, trees$Girth)
abline(coef(lm.fit))
abline(lm.fit) # same as abline(coef(lm.fit))

lm.fit <-lm(Volume ~ Girth, trees)
plot(Volume ~  Girth, trees, main = 'Scatter plot with line of best fit')
abline(lm.fit, col = 'red')
lines(lowess(trees$Girth, trees$Volume), col = 'blue')

poly.fit <- lm(Volume ~ Girth + I(Girth^2), trees)
b <- coef(poly.fit); b
curve(b[1] + b[2]*x + b[3]*x^2, col='gray', add=T, lwd=2)

# make predictions----
# predict(model, newdata, interval = 'prediction', level=.99)
lm.fit <- lm(Volume ~ Girth, trees)
newtrees <- data.frame(Girth=c(17.2, 12.0, 11.4))
predict(lm.fit, newdata = newtrees, interval='prediction')

# with faithful dataset--
head(faithful)
ggplot(faithful, aes(x=waiting, y=eruptions)) + 
  geom_point() +
  geom_smooth(method='lm')

eruption.lm <- lm(eruptions ~ waiting, data=faithful)
summary(eruption.lm)
anova(eruption.lm)

b <- coef(eruption.lm); b
waiting <- 80
duration = b[1] + b[2]*waiting; duration 

newdata <- data.frame(waiting=80)
predict(eruption.lm, newdata)

# with stackloss--
head(stackloss)
colnames(stackloss) <- tolower(colnames(stackloss))

stack.loss.lm <- lm(stack.loss ~ air.flow + water.temp + acid.conc., data = stackloss)
summary(stack.loss.lm)

stack.loss.rlm <- lm(stack.loss ~ air.flow+ water.temp, data = stackloss)
summary(stack.loss.rlm)

anova(stack.loss.lm, stack.loss.rlm)

## predict--
newdata = data.frame(air.flow = 72, water.temp = 20)
predict(stack.loss.rlm, newdata)
predict(stack.loss.rlm, newdata, interval = 'confidence')
predict(stack.loss.rlm, newdata, interval = 'prediction')

## resid(), fitted(), hatvalues(), cooks.distance()--
lm.fit <- lm(Volume ~ Girth, trees)
resids <- rstandard(lm.fit); resids
shapiro.test(resids) # p-value > .05 so residuals' normal distribution
plot(lm.fit, which=2) ## QQ plot :: Normal probability plot (QQ plot) of standardized residuals
plot(lm.fit, which=1) ## residuals against fitted values
plot(lm.fit, which=3) ## similar to the residual against fitted values but it uses the square root of the standardized residuals
plot(lm.fit, which=4); abline(h=4/(31-1+1), col='red') # cook's distance with rule of thumb
plot(lm.fit, which=5); abline(v=2*(1+1)/31, col='blue') # residuals against leverage with rule of thumb
plot(lm.fit, which=6) # cook's distance against leverage

# multicolinearity----
# VIF as an indicator :: the larger the value of VIF, the more 'troublesome' or collinear the variable X
# if the VIF of a variable exceeds 10, which will happen if multiple correlation coefficient 
# for j-th variable R^2 exceeds .9, that variable is said to be highly collinear.

ggpairs(mtcars[, c('mpg', 'disp', 'hp', 'wt', 'drat')])
fit <- lm(mpg ~ disp + hp + wt + drat , data=mtcars)
summary(fit) 
anova(fit)
library(car); vif(fit)

# select best regression model----
head(state.x77)
colnames(state.x77) <- tolower(colnames(state.x77))
glimpse(state.x77); class(state.x77)
states <- as.data.frame(state.x77)
glimpse(states)
states %>%
  rename(life.exp = `life exp`) %>%
  rename(hs.grad = `hs grad`) -> states; head(states)

fit <- lm(murder ~ population + illiteracy + income + frost, data = states)
summary(fit)
anova(fit)

fit1 <- lm(murder ~ population + illiteracy, data=states)
fit2 <- lm(murder ~ population + illiteracy + income + frost, data=states)
anova(fit1, fit2)

AIC(fit1, fit2) # model with small AIC values (indicating adequate fit with fewer parameters) are prefered

# stepwise selection
fit1 <- lm(murder ~ 1, data=states)
fit2 <- lm(murder ~ population + illiteracy + income + frost, data=states)

# library(MASS)
stepAIC(fit2, direction = 'backward')
stepAIC(fit1, direction = 'forward', scope=list(lower=fit1, upper=fit2))
stepAIC(fit1, direction= 'both', scope=list(lower=fit1, upper=fit2))


write.csv(bio, './data/bio.csv', row.names = F)
bio <- read.csv('./data/bio.csv')
head(bio)
step(lm(pemax~1, bio), scope=list(lower=~1, upper=~age+weight+bmp+rv+frc+tlc), direction = 'forward')
step(lm(pemax~age+weight+bmp+rv+frc+tlc, bio), direction = 'backward')
step(lm(pemax~1, bio), scope=list(lower=~1, upper=~age+weight+bmp+rv+frc+tlc), direction = 'both')

# all-subsets regression
library(leaps)
leaps <- regsubsets(murder ~ population + illiteracy + income + frost, data=states, nbest=4)
plot(leaps, scale='adjr2') # scale = 'Cp', 'adjr2', 'r2', 'bic'

leaps <- regsubsets(pemax ~ age + weight + bmp + rv + frc + tlc, data=bio, nbest=6)
plot(leaps, scale = 'adjr2')

# standardized regression coef
fit1 <- lm(murder ~ population + illiteracy, data=states); summary(fit1)
fit2 <- lm(scale(murder) ~ scale(population) + scale(illiteracy), data=states); summary(fit2)











