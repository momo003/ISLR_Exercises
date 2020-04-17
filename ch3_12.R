set.seed(1)

## b   (diff coeff)

x = rnorm(100)
y = 2*x + rnorm(100)

lm.fit = lm(y~x+0)
summary(lm.fit)

lm.fit = lm(x~y+0)
summary(lm.fit)

## c    (same coeff)
x = rnorm(100, mean=500, sd=0.1)
y = rnorm(100, mean=500, sd=0.1)

lm.fit = lm(y~x+0)
summary(lm.fit)

lm.fit = lm(x~y+0)
summary(lm.fit)


