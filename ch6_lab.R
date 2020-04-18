
library(ISLR)
#fix(Hitters)
attach(Hitters)
names(Hitters)
dim(Hitters)

## find observations missing a Salary value
sum(is.na(Salary))
## omit obs. missing values for any feature
Hitters = na.omit(Hitters)
dim(Hitters)

## any obs. missing any feature
sum(is.na(Hitters))


## Best subset selection
library(leaps)

## upto 8 features in models  (default option)
regfit.full = regsubsets(Salary ~ . , Hitters)
summary(regfit.full)

## upto 19 features in models
regfit.full = regsubsets(Salary ~ . , Hitters, nvmax = 19)
reg.summary = summary(regfit.full)

## check for best overall model (based on R2, RSS, adj. R2, Cp, BIC)
names(reg.summary)

##  R2 increases monotonically with the nr. of features,
##  (from 32.14 % for 1-feature model to 54.61% for all-features (19) model)
reg.summary$rsq

##  adj. R2 is a better measure, not biased with nr. of features
## the larger, the better
reg.summary$adjr2
which.max(reg.summary$adjr2)

## plot all R2, RSS, adj. R2, Cp, BIC measures for all models
par(mfrow = c(2,2))

plot(reg.summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "b")
plot(reg.summary$adjr2, xlab = "Number of variables", ylab = "Adjusted Rsq", type = "b")

##  put points on graphs, using points()
##  here put point with max(adj. Rsq)
ind = which.max(reg.summary$adjr2)
points(ind, reg.summary$adjr2[ind], col = "red", cex = 2, pch = 20)

plot(reg.summary$cp, xlab = "Number of variables", ylab = "Cp", type = "b")
ind = which.min(reg.summary$cp)
points(ind, reg.summary$cp[ind], col = "red", cex = 2, pch = 20)

plot(reg.summary$bic, xlab = "Number of variables", ylab = "BIC", type = "b")
ind = which.min(reg.summary$bic)
points(ind, reg.summary$bic[ind], col = "red", cex = 2, pch = 20)


## use built-in plot command of regsubsets() function
##  to interpret the following plots, a black box means that feature is included in model.
##  e.g. based on BIC plot, a model with 6 features (AtBat, Hits, Walks, CRBI, DivisionW, PutOuts)
##  has lowest BIC (-150), and is the best model
plot(regfit.full, scale = "r2")
plot(regfit.full, scale = "adjr2")
plot(regfit.full, scale = "Cp")
plot(regfit.full, scale = "bic")

## get coef of model with 6 features (the best model)
coef(regfit.full, 6)




## Forward and Backward Stepwise Selection
regfit.fwd = regsubsets(Salary ~ . , data = Hitters, nvmax = 19, method = "forward")
summary(regfit.fwd)

regfit.bwd = regsubsets(Salary ~ . , data = Hitters, nvmax = 19, method = "backward")
summary(regfit.bwd)

## check which features are incldued in models from diff. subset selections, ind: nr. of features
ind = 6
coef(regfit.full, ind)
coef(regfit.fwd, ind)
coef(regfit.bwd, ind)


##
## Choosing among models using validation set approach and cross-validation
## (instead of BIC, Cp, adj. R2)
##

##
## we should use train set and not the whole data set for these methods
## to get accurate estimate of test error

set.seed(1)
train = sample(c(TRUE, FALSE), nrow(Hitters), rep = TRUE)
test  = (!train)

## perform best subset selection on train data
regfit.best = regsubsets(Salary ~ ., data = Hitters[train,], nvmax = 19 )

## calculate validation set error 
##  firts build an X matrix from data
test.mat = model.matrix(Salary ~ . , data = Hitters[test,])

## make a loop on model sizes, and for each size extract the coefs from the regfit.best (done on train set)
## for the best model of that size. Then we multiply these coefs into the appropriate columns of the test model
## matrix to form the predictors. Using these predictors we compute MSE for test data which is equivalent to test error.

val.errors = rep(NA, 19)
for (i in 1:19) {
  coefi = coef(regfit.best, id = i)
  pred  = test.mat[, names(coefi)]%*%coefi
  val.errors[i] = mean((Hitters$Salary[test] - pred)^2)
}

val.errors
which.min(val.errors)
coef(regfit.best, which.min(val.errors))

##  IN ISLR BOOK THE BEST MODEL IS THE 10-Varibales MODEL
##  I found it to be the 7-Variables model.

# i=10
# coefi = coef(regfit.best, id = i)
# names(coefi)
# test.mat[, names(coefi)]



##   a function to do the prediction part (as done above)
##   the only problem is how to how extracted the formula
##   used in the call to regsubset()
##   how to use the above function? 
##   We show when doing cross-validation later
predict.regsubsets = function(object, newdata, id, ...){
  form  = as.formula(object$call[[2]])
  mat   = model.matrix(form, newdata)
  coefi = coef(object, id = id)
  xvars = names(coefi)
  mat[,xvars]%*%coefi
}



##  Above We found out how many varibales are in the best model.
##  Now perform best subset selection on Full Data, and select 
##  the best 7-variable model (found best above)
##  We use full data to get accurate values of coefs.
##  We DO NOT use the coefs found for the best model (7-vars. model) 
##  in best subset selection done on training set. But we use full model
##   because the coefs can differ. 


regfit.best = regsubsets(Salary ~ ., data = Hitters, nvmax = 19 )
nv = which.min(val.errors)
coef(regfit.best, nv)

##  Notice the difference between variables in best 7-variables 
##  model here and the one found for training set


##  Cross Validation approach
##    Cross Validation approach for choosing among models of different sizes
##    it is to do best subset selection for each of the k training sets


##   allocate each obs. to  one of the 10 folds
k = 10
set.seed(1)
folds = sample(1:k, nrow(Hitters), replace = TRUE)
## matrix for storing results
cv.errors = matrix(NA, k, 19, dimnames = list(NULL, paste(1:19)))


## do cross-validation on folds in a loop,
## in each j'th step, the j'th fold is the test 
## and the rest folds are training set
## Prediction is done using our own function declared above
## the computet test error (calculated from prediction on test set)
##  is stored
for (j in 1:k) {
  best.fit = regsubsets(Salary ~ . , data = Hitters[folds !=j,],
                        nvmax = 19)
  for (i in 1:19) {
    pred = predict(best.fit, Hitters[folds == j,], id = i)
    cv.errors[j,i] = mean( (Hitters$Salary[folds == j] - pred)^2)
  }
}
##  cv.errors will be a (7x19) matrix, where (i,j) element
##  is the test MSE for the i'th cross-validation fold for
##  the best j-variables model



##  average over columns of the matrix, to obtain a vector for which
##  the j'th element is the cross-validation error for the j-variables model

mean.cv.errors = apply(cv.errors, 2, mean)
mean.cv.errors
## Which model is the best (i.e How many variables are in the best model?)
which.min(mean.cv.errors)


## Note that the results is different from the valdiation set approach


## Now, perform best subset selection on the full data, to get the x-variable model
## (found above to be the best)

reg.best = regsubsets(Salary ~ . , data = Hitters, nvmax = 19)
coef(reg.best, which.min(mean.cv.errors))



##


# Ridge Regression and the Lasso

## 
















