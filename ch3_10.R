## exercise 10
#fix(Carseats)

lm.fit = lm(Sales~Price+Urban+US)
summary(lm.fit)

lm.fit.mod = lm(Sales~Price+US)
summary(lm.fit.mod)

## assessing accuracy:
##  large RSE : model does not fit well the dat
##  large R^2 : model fits well, since it can explain large portion of varaince in data

## 95% confidence interval of coefficient estimates
confint(lm.fit)
confint.default(lm.fit.mod)

## diagnostic plot for fit, to find outliers or high leverage points
pdf('exercise_10.pdf')
par(mfrow=c(2,2))
plot(lm.fit)
par(mfrow=c(2,2))
plot(lm.fit.mod)
dev.off()