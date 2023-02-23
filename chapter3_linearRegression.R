rm(list = ls())
library(MASS)
library(ISLR)
library(carData)
library(car)


fix(Boston)
names(Boston)
lm.fit <- lm(medv~lstat) # Object " medv" not found
lm.fit <- lm(medv~lstat, data = Boston)
summary(lm.fit)
attach(Boston)
lm.fit <- lm(medv~lstat) # no bug
names(lm.fit)
coef(lm.fit)
confint(lm.fit) # confidence intervals of model parameters
predict(lm.fit, data.frame(lstat=(c(5, 10, 15))), interval = "confidence")
predict(lm.fit, data.frame(lstat=(c(5, 10, 15))), interval = "prediction")
plot(Boston$lstat, Boston$medv) # scatter plot
abline(lm.fit) # add straight lines to a plot
plot(Boston$lstat, Boston$medv, pch = 1: 20, ) # pch-use different symbols
# 2 x 2 figure
par(mfrow=c(2, 2), mar=c(1, 1, 1, 1)) # mar control the margin
plot(lm.fit)

plot(predict(lm.fit), residuals(lm.fit)) # residuals figure has some pattern which means no-linearity
plot(predict(lm.fit), rstudent(lm.fit))
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))

mlm.fit <- lm(medv~lstat+age, data = Boston)
summary(mlm.fit)
mlm.fit <-  lm(medv~., data = Boston)
summary(mlm.fit)
names(summary(mlm.fit))
names(mlm.fit)
summary(mlm.fit)$sigma
vif(mlm.fit) # > 10 means terriable

mlm.fit1 <- update(mlm.fit, ~. -age) # euqal mlm.fit1 <- lm(medv~. -age, data=Boston)
summary(mlm.fit1)

# interaction terms
summary(lm(medv~lstat*age, data = Boston)) # lstat+age+lstat:age
# no-linear transformations of the predictors
summary(lm(medv~lstat+I(lstat^2), data = Boston))
lm.fit <- lm(medv~lstat, data = Boston)
lm.fit2 <- lm(medv~lstat+I(lstat^2), data = Boston)
anova(lm.fit, lm.fit2) # h0=the two models fit the data equally well, h1=the second model is superior
par(mfrow=c(2, 2))
plot(lm.fit2)
# high order polynomials
lm.fit5 <- lm(medv~poly(lstat, 5), data = Boston)
summary(lm.fit5)
summary(lm(medv~log(rm), data = Boston))

# qualitative predictors
fix(Carseats)
names(Carseats)
lm.fit <- lm(Sales~.+Income:Advertising+Price:Age, data = Carseats)
summary(lm.fit)
names(summary(lm.fit))
contrasts(Carseats$ShelveLoc)

# exercises 8
fix(Auto)
lm.fit <- lm(mpg~horsepower, data = Auto)
summary(lm.fit)
confint(lm.fit)
plot(Auto$mpg, Auto$horsepower)
plot(lm.fit)
