## doing t-stat for null Hypoth.: Beta = 0 in simple linear rerg. w/o intercept

## generate data points and response
set.seed(1)
x = rnorm(100)
y = 2 * x + rnorm(100)

lm.fit.yx = lm(y~ x + 0 )
summary(lm.fit.yx)

lm.fit.xy = lm(x ~ y+0)
summary(lm.fit.xy)


## part f

x = rnorm(100)
y = rnorm(100)
lm.fit = lm(y~x + 0)
summary(lm.fit)
lm.fit = lm(x~y + 0)
summary(lm.fit)