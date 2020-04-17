## Validation set approach

library(ISLR)
attach(Auto)

set.seed(1)
train = sample(392, 196)

##  fit different models (linear, polynomial of 2nd and 3rd oderes) on train set
##  predict on 'ALL' data and calculate MSE on Validation set to find test error

##   Linear Regression Model
lm.fit = lm(mpg ~ horsepower, data = Auto, subset = train)
mpg.pred = predict(lm.fit, Auto)      ## on all data
## Test error (MSE on Validation)
mean((mpg - mpg.pred)[-train]^2)

##  Polynomial (quadratic) Regression Model 
lm.fit2 = lm(mpg ~ poly(horsepower, 2), data = Auto, subset = train)
mpg.pred2 = predict(lm.fit2, Auto)
## Test error (MSE on Validation)
mean((mpg - mpg.pred2)[-train]^2)

##  Polynomial (Cubic) Regression Model
lm.fit3 = lm(mpg ~ poly(horsepower, 3), data = Auto, subset = train)
mpg.pred3 = predict(lm.fit3, Auto)
## Test error (MSE on Validation)
mean((mpg - mpg.pred3)[-train]^2)



##   Leave-One-Out Cross-Validation (loocv)
##   loocv is possible using glm() and cv.glm()
##   glm is the generalized lm
##   glm() with giving "family=binomial" performs logit
##   glm() without giving "family=binomial" performs exactly  lm()
##   cv.glm() is in boot library
library(boot)
glm.fit = glm(mpg ~ horsepower, data = Auto)
cv.err  = cv.glm(Auto, glm.fit)

## CV estimate for Test MSE 
## ( average of n test error, i.e. average of all MSE_i)
cv.err$delta        

## NOTE:
## delta is vector of length 2. for LOOCV both are same
## but in k-fold CV the first element of delta is estimated test error
## i.e. av. of MSE_i's, and the second one is the bias corrected version


## increasing complexity of polynomial fits in a for loop
cv.error = rep(0,5)
for (i in 1:5) {
  glm.fit = glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.err = cv.glm(Auto, glm.fit)
  cv.error[i] = cv.err$delta[1]
}
cv.error


##    k-fold Cross-Validation
set.seed(17)
cv.error.10 = rep(0,10)
cv.error.10.2 = rep(0,10)
for (i in 1:10) {
  glm.fit = glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.err = cv.glm(Auto, glm.fit, K = 10)
  cv.error.10[i] = cv.err$delta[1]     ## estimate for test error
  cv.error.10.2[i] = cv.err$delta[2]   ## bias corrected estimate
}
cv.error.10
cv.error.10.2


##       Bootstrap
##  for bootstrap 
##    1) we need a function that calculates the statistics
##    2) call boot() to perform bootstrap by repeated sampling
##    belwo first we make the function and then manually do bootsrap
##    but we also perform bootstrap using boot() function



attach(Portfolio)


alpha.fn = function(data, index){
  X = data$X[index]
  Y = data$Y[index] 
  return((var(Y) - cov(X,Y)) / (var(X) + var(Y) - 2*cov(X,Y)))
}


##  ----  manual bootstrap --------------
## calculate aplha using (all) 100 obs. in Portfolio
alpha.fn(Portfolio, 1:100)

##  randomly select another 100 obs. with replacement out of all obs.
##  that is constructing a new bootstrap data set 
## recomputing alpha using this set
set.seed(1)
alpha.fn(Portfolio, sample(100,100, replace = T))
## -----------------------------------------


##  perform bootstrap using R function boot()
## produce 1000 bootstrap estimates fo alpha
boot(Portfolio, alpha.fn, R = 1000)




##  Use Bootstrap to estimate the Accuracy of Model (here linear reg. model)
##  Here we compare SE of coefs using bootstrap to those calculated from
##  formulas for SE (in ISLR sec 3.1.2)

## function to perform linear reg and retruning coefs of fit
boot.fn = function(data, index){
  return(coef(lm(mpg ~ horsepower, data = data, subset = index)))
}

boot.fn(Auto, 1:392)

##  perform manually two bootstraps ---------
set.seed(1)
boot.fn(Auto, sample(392, 392, replace = T))

boot.fn(Auto, sample(392, 392, replace = T))
## -------------------------------------

##   perform bootstrap 1000 times to get the estimate for coef's SE
boot(Auto, boot.fn, R=1000)


##  perform lm and get SE from formulas
lm.fit = lm(mpg~horsepower, data = Auto)
summary(lm.fit)$coef



##  Accuracy of linear regression with quadratic terms
boot.fn = function(data, index){
  return(coef(lm(mpg ~ horsepower + I(horsepower^2), data = Auto,
                 subset = index)))
}

set.seed(1)
boot(Auto, boot.fn, R=1000)

lm.fit = lm(mpg ~ horsepower + I(horsepower^2), data = Auto)
summary(lm.fit)$coef

