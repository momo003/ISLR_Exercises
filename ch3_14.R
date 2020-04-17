setwd('~/Documents/R/ISLR')
pdf('ch3_14.pdf')

## a
set.seed(1)
x1 = rnorm(100)
x2 = 0.5 * x1 + rnorm(100)/10
y = 2 + 2 * x1 + 0.3 * x2 + rnorm(100)

## b
cor(x1, x2)
plot(x1, x2)

## c
lm.fit = lm(y~x1+x2)
summary(lm.fit)

## d
lm.fit.x1 = lm(y~x1)
summary(lm.fit.x1)

## e
lm.fit.x2 = lm(y~x2)
summary(lm.fit.x2)

## f: part c says we should reject H0 for X2, but e is opposing it

## g    (add new (missing) data point)
x1 = c(x1, 0.1)
x2 = c(x2, 0.8)
y  = c(y, 6)

lm.fit.new = lm(y~x1+x2)
summary(lm.fit.new)

lm.fit.x1.new = lm(y~x1)
summary(lm.fit.x1.new)

lm.fit.x2.new = lm(y~x2)
summary(lm.fit.x2.new)

par(mfrow=c(2,2))
plot(lm.fit.new)

par(mfrow=c(2,2))
plot(lm.fit.x1.new)

par(mfrow=c(2,2))
plot(lm.fit.x2.new)


dev.off()