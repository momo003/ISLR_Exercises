LoadLibraries = function(){
  library(ISLR)
  library(MASS)
}

setwd(getwd())
LoadLibraries()
# car library include function for calculating variance inflation factor
library(car)

# fit use two: age and lstat
lm.fit = lm(medv ~ lstat + age, data = Boston)

# use all predictors
lm.fit = lm(medv ~ . , data = Boston)
summary(lm.fit)

# R^2 and RSE of fit
summary(lm.fit)$r.sq
summary(lm.fit)$sigma

# variance inflation factor (VIF), measure for multi-collinearity
vif(lm.fit)

# use all preditors except age
lm.fit.no.age = lm(medv~.-age, data = Boston)
summary(lm.fit.no.age)

# we can use "update" to update a fit after some changes
lm.fit.no.age.indus = lm(medv~.-indus-age, data = Boston)
summary(lm.fit.no.age.indus)

## include interaction terms in linear model 
## (lstat*age : inlcudes lstat, age, and (lstat x age) together)
lm.fit.age.lstat = lm(medv~lstat*age, data = Boston)
summary(lm.fit.age.lstat)

## include interaction terms in linear model
## (lstat*age : inlcudes  lstat x age)
lm.fit.age.lstat.2 = lm(medv~lstat:age, data = Boston)
summary(lm.fit.age.lstat.2)


## nonlinear transformation of predictors  (includ: x + x^2)
lm.fit2 = lm(medv ~ lstat + I(lstat^2), data = Boston)
summary(lm.fit2)

## compare simple fit with fit to transformations
## anova() function performs hypothesis test (null: both are)
## equally good, alternative : the second one is better
## large F and small p-vals means the alternative is valid
lm.fit.lstat = lm(medv~lstat, data = Boston)
anova(lm.fit.lstat, lm.fit2)

pdf('ch3.pdf')
par(mfrow = c(2,2))
plot(lm.fit2)
dev.off()

# make polynomial fit (order 5
lm.fit.poly5 = lm(medv~poly(lstat, 5), data = Boston)
summary(lm.fit.poly5)
anova(lm.fit, lm.fit.poly5)

lm.fit.poly6 = lm(medv~poly(lstat, 6), data = Boston)
summary(lm.fit.poly6)
anova(lm.fit.poly5, lm.fit.poly6)

# log transfromation of predictor "rm" 
lm.fit.log.rm = lm(medv~log(rm), data = Boston)
summary(lm.fit.log.rm)
