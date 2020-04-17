setwd('~/Documents/R/ISLR')
library(ISLR)

pdf('ch4_lab.pdf')

pairs(Smarket)

## correlation matrix
cor(Smarket[,-9])

plot(Smarket$Volume)


## ----- Logestic Regression  response: Direction -----


## --- tarin and test on same data ---
##             train
glm.fit = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
              data = Smarket, family = binomial)
summary(glm.fit)
coef(glm.fit)
f
## --- what coding R used for Directions ---
contrasts(Smarket$Direction)

## --- Predict on the same data ---
## --- Predict prob. of 'Direction' going up ---
## --- No data set is passed to 'predict' --> Predictions are for the training data
## --- type = "response" : Give probs. in the form Pr(Y=1|X)

glm.probs = predict(glm.fit, type = "response")   

## --- first 10 predictions ---
glm.probs[1:10]

## --- Convert probs. to Up/Down ---
glm.pred = rep("Down", 1250)
glm.pred[glm.probs > 0.5] ="Up"

## --- Confusion matrix ---
table(glm.pred, Smarket$Direction)

## --- Fraction of days for which prediction was correct ---
mean(glm.pred == Smarket$Direction)

## --- same as using mean ---
a = table(glm.pred, Smarket$Direction)
(a[1,1]+a[2,2])/1250




## --- train on a part of data and test on held out part  ---

## --- partition data into train and test ---
## --- Create a bolean vector: TRUE if Year < 2005 ---
train = (Smarket$Year < 2005) 
Smarket.2005 = Smarket[!train,]
dim(Smarket.2005)
Direction.2005 = Smarket$Direction[!train]

## --- Fit logit to train set (subset = train set) ---
glm.fit = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
              data = Smarket, family = binomial, subset = train)

## --- Predict for test set ---
glm.probs = predict(glm.fit, Smarket.2005, type = "response", )

## --- Change results from Prob. Vals. to Up/Down ---
glm.pred = rep("Down", length(glm.probs))
glm.pred[glm.probs > 0.5] = "Up"

## --- Confusion Matrix ---
table(glm.pred , Direction.2005)

## --- fraction of correct prediction
mean(glm.pred == Direction.2005)

## --- error rate (fraction of incorrect predictions)
mean(glm.pred != Direction.2005)



## ----- LDA  Linear Discriminant Analysis -----
library(MASS)

##       train      
lda.fit = lda(Direction ~ Lag1 + Lag2, data = Smarket,
              subset = train)
lda.fit

plot(lda.fit)

##      test on test data
lda.pred = predict(lda.fit, Smarket.2005)
names(lda.pred)

## --- class contains the results
lda.class = lda.pred$class

## --- Confusion Matrix ---
table(lda.class, Direction.2005)

## --- Fraction of days for which prediction was correct 
mean(lda.class == Direction.2005)

## --- error rate (fraction of incorrect predictions)
mean(lda.class != Direction.2005)

## --- Use posterior probs. (> 50 %) to re-create the results in lda.pred$class
sum(lda.pred$posterior[,1] >= 0.5)
sum(lda.pred$posterior[,1] < 0.5)

## the model (lda.pred$class outputs only the decrease)
lda.pred$posterior[1:20, 1]
lda.class[1:20]

## use other theresholds for probs., e.g. market decreases prob < 90% 
sum(lda.pred$posterior[,1] >= 0.90)



## ----- Quadratic Discriminant Analysis QDA -----

##   train
qda.fit = qda(Direction ~ Lag1+Lag2, data = Smarket, subset = train)
qda.fit

##   test
qda.class = predict(qda.fit, Smarket.2005)$class
table(qda.class, Direction.2005)

## --- fraction of correct predictions
mean(qda.class == Direction.2005)



## ----- KNN -----
library(class)
train.X = cbind(Smarket$Lag1,Smarket$Lag2)[train,]
test.X  = cbind(Smarket$Lag1,Smarket$Lag2)[!train,]
train.Direction = Smarket$Direction[train]



set.seed(1)

K = 3
knn.pred = knn(train.X, test.X, cl = train.Direction, k = K)
table(knn.pred, Direction.2005)

## --- fraction of correct predictions
mean(knn.pred == Direction.2005)


dev.off()



## ----- KNN for Caravan Insurance Data -----

dim(Caravan)
attach(Caravan)
summary(Purchase)

## --- standardize data to eliminate the influence of differnt scaling on results ---
## --- standardized all data to have mean = 0 and sd = 1  (except qualitative data)
standardized.X = scale(Caravan[,-86])
var(Caravan[,1])
var(Caravan[,2])
var(standardized.X[,1])
var(standardized.X[,2])

## --- first 1000 obs. as test, rest as train set
test = 1:1000

## --- take "standardized.X" with indices NOT in seq. 1:1000
train.X = standardized.X[-test,]

## --- take "standardized.X" with indices in seq. 1:1000
test.X  = standardized.X[test,]

train.Y = Purchase[-test]
test.Y  = Purchase[test]

set.seed(1)

K = 5
knn.pred = knn(train = train.X, test = test.X, cl = train.Y, k = K)

## --- error rate of trained KNN on test set
mean(test.Y != knn.pred)

## --- % of Purchase != "No" (i.e. Purchase = Yes) in original (real) data
mean(test.Y != 'No')
##     Two Interpretations
##     if we always predict "NO", error rate would be 6 %. 
##     this means if we randomly select customers to sell them,
##     our success will be 6%







## ---  BUT in this kind of problems we may be mainly interested in the
##      fraction of correctly predicted purchases and not the error rate

table(knn.pred , test.Y)



## ----- logistic regression on Caravan -----

glm.fit = glm(Purchase ~. , data = Caravan, family = binomial, subset = -test)
glm.probs = predict(glm.fit, Caravan[test,], type = "response")

## --- use a prob. of 50 % for success
glm.pred = rep("No", 1000)
glm.pred[glm.probs > 0.5] = "Yes"
## only 7 are predicted as success, even these are false predictions
table(glm.pred, test.Y)

## --- use a prob. of 25 % for success
glm.pred = rep("No", 1000)
glm.pred[glm.probs > 0.25] = "Yes"

knn.table = table(glm.pred, test.Y)

print(knn.table)

## --- fraction of correct predictions
knn.table[2,2]/(knn.table[2,1]+knn.table[2,2])



  