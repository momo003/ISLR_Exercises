
library(MASS)
library(ISLR)

# fix(Boston)
names(Boston)

## simple linear regression
lm.fit = lm(medv ~ lstat, data = Boston)
summary(lm.fit)
names(lm.fit)
lm.fit$coefficients
coef(lm.fit)

## get confidence interval for coeff. estimates
confint(lm.fit)

## making prediction

## predict confidence intervals for medv for given lstat values, related to (i.e. a measure for) reducible error
predict(lm.fit, data.frame(lstat = (c(5, 10, 15))), interval = "confidence")

## predict prediction interval for medv for given lstat values, related to (i.e. a measure for) irredcible erro
predict(lm.fit, data.frame(lstat = (c(5, 10, 15))), interval = "prediction")

## plot data and regression line together
plot(Boston$lstat, Boston$medv, col = "red", pch = 20)
abline(lm.fit, lwd = 3, col = "red")

## diagnostic plots for regression
par(mfrow = c(2,2))
plot(lm.fit)

## is linear model enough (is linear assumption Ok for the given data)? Plot residuals of regression fit (difference between observed data y and the fitted values y_hat) and studentized residuals vs fitted values. (if no structure is seen, linear assumption is good enough)
par(mfrow = c(2,1))
plot(predict(lm.fit), residuals(lm.fit), xlab = "Prediction", ylab = "Residuals")
plot(predict(lm.fit), rstudent(lm.fit), xlab = "Prediction", ylab = "Studentized Residuals")

# Leverage statistics for any number of predictors observation, which observation has max. leverage
plot(hatvalues(lm.fit), xlab = "Observation", ylab = "Leverage")
which.max(hatvalues(lm.fit))


