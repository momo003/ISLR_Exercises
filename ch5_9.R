library(MASS)
library(boot)

attach(Boston)


##   a
mu.hat = mean(medv)
print(mu.hat)

##   b

## SE of mean
sd(medv)/sqrt(dim(Boston)[1])

##  c

###  standard error (SE) of the mean of data, using bootstrap 
meanFunc = function(x,i){mean(x[i])}
boot.mean= boot(Boston$medv, meanFunc, R=100)

## standard error se of mean calculated by boot strap 
boot.mean.se = sd(boot.mean$t)

##  d
## interval calculated using boot strap estimate of standard error (se)
left = mu.hat - 2*boot.mean.se
right = mu.hat + 2*boot.mean.se
left
right

## do test using built-in function to get intervals
t.test(medv)

##  e
median(medv)

##   f
##  NO simple formula for se of median --> use boot strap
##  use  boot strap to calculate standard error of median
##  (there is unfortunately no simple formula for se of median,
##   therefore we use boot strap)
medianFunc = function(x,i){median(x[i])}
boot.median = boot(Boston$medv, medianFunc, R=100)
boot.median.se = sd(boot.median$t)
boot.median.se


##   g
## 10-th quantile 
quantile(medv, probs = 0.1)

## 0-100 percentile
#quantile(medv, probs = seq(0,1,0.1))


##   h
quantFunc = function(x,i){quantile(x[i], probs = 0.1)}
boot.quantile = boot(medv, quantFunc, R=10)

boot.quantile
## SE of 10th quantile calculated by boot strap
sd(boot.quantile$t)