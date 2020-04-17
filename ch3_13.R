set.seed(1)

pdf('ch3_13.pdf')
## a
x = rnorm(100, mean = 0, sd = 1)

## b
  eps = rnorm(100, mean = 0, sd = 0.9)

## c
y = -1.0 + 0.5 * x + eps

## d
plot(x,y, col = 3)

## e
lm.fit = lm(y~x)
summary(lm.fit)

## f
abline(lm.fit, lwd = 3, col =4)
legend(-2.35, 0.45, 
       legend = c("data point", "linear fit"),
       col =c(3, 4),
       lty = 1:2,
       cex = 0.7)

## g
lm.pol = lm(y~ x + I(x^2))
summary(lm.pol)

#lm.pol = lm(y~poly(x,2))
#summary(lm.pol)

## h: decrease sd in eps  (line 8)
## j: increase sd in eps  (line 8)
confint(lm.pol)

dev.off()