setwd('~/Documents/R/ISLR')
library(MASS)
library(ISLR)
pdf("ch3_applied_exercise.pdf")

## exercise 8

lm.fit = lm(mpg ~ horsepower, data = Auto)
summary(lm.fit)

## 95% confidence interval of coefficient estimates
confint(lm.fit)

## confidence intervals for prediction of mpg
predict(lm.fit, data.frame(horsepower=(c(5,10,15,20))),
        interval = "confidence")

## prediction intervals for prediction of mpg
res = predict(lm.fit, data.frame(horsepower=(c(5,10,15,20))),
              interval = "prediction")


plot(horsepower, mpg,  col = 3)
abline(lm.fit, lwd = 3, col = "red")
par(mfrow= c(2,2))
plot(lm.fit, col = 4)

## exercise 9
pairs(Auto)

## remove column 'names' from dataset
Auto = Auto[,-9]
cor(Auto, Auto)
lm.fit.multi = lm(mpg~. , data = Auto)
summary(lm.fit.multi)
par(mfrow = c(2,2))
plot(lm.fit.multi, col = "green")
names(Auto)
lm.fit.interaction = lm(mpg~ displacement*acceleration)
summary(lm.fit.interaction)

lm.fit.trans = lm(mpg~horsepower + I(horsepower^2))
summary(lm.fit.trans)

lm.fit.trans2 = lm(mpg~poly(weight,2))
summary(lm.fit.trans2)

lm.fit.log = lm(mpg~log(displacement), data = Auto)
summary(lm.fit.log)

lm.fit.sq = lm(mpg~sqrt(horsepower), data = Auto)
summary(lm.fit.sq)




dev.off()