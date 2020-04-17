setwd('~/Documents/R/ISLR')
library(ISLR)
names(Auto)
attach(Auto)
cor(Auto[,-9])

##     a
## --- create new variable 0/1 if </> median(mpg)
mpg01 = rep(1, dim(Auto)[1])
mpg01[mpg <= median(mpg)] = 0
##     an alternative method:  mpg01 = ifelse(mpg > median(mpg), 1, 0)
Mauto = data.frame(Auto, mpg01)
attach(Mauto)
detach(Auto)


##     b
## --- build new df (Auto + mpg01)
pairs(Mauto)
#boxplot(Mauto[,-1])
#boxplot(Mauto[seq(1,5)])


##     c
## --- select 70 % of data as train set, randomly
set.seed(1)
trainid = sample(1:nrow(Mauto), nrow(Mauto)*0.7, replace = F)
train = Mauto[trainid,]
test  = Mauto[-trainid,]


##     d 
library(MASS)

lda.fit = lda(mpg01 ~ displacement + horsepower + weight + acceleration,
              data = train)
#lda.fit
#plot(lda.fit)

lda.pred = predict(lda.fit, test)
names(lda.pred)
lda.class = lda.pred$class

## --- Confusion Matrix ---
table(lda.class, test$mpg01)

## --- Fraction of correct predictions for mpg01
mean(lda.class == test$mpg01)

## --- error rate (fraction of incorrect predictions), i.e. test error
mean(lda.class != test$mpg01)


##     e 
## 
qda.fit = qda(mpg01 ~ displacement + horsepower + weight + acceleration,
              data = train)
qda.fit

qda.pred = predict(qda.fit, test)
qda.class = qda.pred$class

## --- Confusion Matrix ---
table(qda.class, test$mpg01)

## --- Fraction of d correct predictions for mpg01
mean(qda.class == test$mpg01)

## --- error rate (fraction of incorrect predictions), i.e. test error
mean(qda.class != test$mpg01)


##     f
glm.fit = glm(mpg01 ~ displacement + horsepower + weight + acceleration,
              data = train, family = binomial)


glm.probs = predict(glm.fit, test, type = "response" )

## --- Change results from Prob. Vals. to Up/Down ---
glm.pred = rep(0, length(glm.probs))
glm.pred[glm.probs > 0.5] = 1

## alternative way:  glm.pred <- ifelse(glm.probs > 0.5, 1, 0)

## --- Confusion Matrix ---
table(glm.pred , test$mpg01)

## --- Fraction of d correct predictions for mpg01
mean(glm.pred == test$mpg01)

## --- error rate (fraction of incorrect predictions), i.e. test error
mean(glm.pred != test$mpg01)

##     g

## ----- KNN -----
library(class)
train.X = cbind(train$displacement, train$horsepower, 
                train$weight, train$acceleration)
test.X  = cbind(test$displacement, test$horsepower, 
                test$weight, test$acceleration)
train.Direction = train$mpg01



set.seed(1)

K = 3
knn.pred = knn(train.X, test.X, cl = train.Direction, k = K)
table(knn.pred, test$mpg01)

## --- fraction of correct predictions
mean(knn.pred == test$mpg01)

## --- fraction of incorrect predictions, i.e. error rate
mean(knn.pred != test$mpg01)
