setwd('~/Documents/R/ISLR')
##    a
set.seed(1)
y = rnorm(100)
x = rnorm(100)
y = x - 2*x^2 + rnorm(100)

##    b

par(col.lab = 1,
    cex = 1.2,
    cex.axis = 1.2,
    cex.lab = 1.2,
    tck = 0.02
    )

plot(x,y, col=1)


##    c & d
set.seed(3)
library(boot)

df = data.frame(y, x, x2 = x^2, x3 = x^3, x4 = x^4)

fit.1 = glm(y~x, data = df)
fit.1.error = cv.glm(df, fit.1)
fit.1.error$delta[1]

fit.2 = glm(y~ x + x2, data = df)
fit.2.error = cv.glm(df, fit.2)
fit.2.error$delta[1]

fit.3 = glm(y~ x + x2 +x3, data = df)
fit.3.error = cv.glm(df, fit.3)
fit.3.error$delta[1]

fit.4 = glm(y~ x + x2 + x3 + x4, data = df)
fit.4.error = cv.glm(df, fit.4)
fit.4.error$delta[1]


##    e
### model ii has the lowest, it is clear due to
### the quadratic dependence in model and data

##    f

coef(fit.1)

coef(fit.2)

coef(fit.3)

coef(fit.4)