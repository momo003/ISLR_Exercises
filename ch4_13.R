library(MASS)

attach(Boston)
names(Boston)
median(crim)
dim(Boston)
cor(Boston)

## --- make a binary variable for crime
crim01 = ifelse(crim > median(crim), 1, 0)
NBoston = data.frame(Boston, crim01)
attach(NBoston)
detach(Boston)

pairs(NBoston)

## --- select train and test sets
set.seed(1)
trainid = sample(1:nrow(NBoston), nrow(NBoston)*0.7, replace = F)
train = NBoston[trainid,]
test  = NBoston[-trainid,]


## --- linear discriminant analysis  (LDA)

lda.fit = lda(crim01 ~ zn + indus + nox + rm +  rad  ,
              data = train)

lda.pred = predict(lda.fit, test)
names(lda.pred)
lda.class = lda.pred$class

## --- Confusion Matrix ---
table(lda.class, test$crim01)

## --- Fraction of correct predictions for mpg01
mean(lda.class == test$crim01)

## --- error rate (fraction of incorrect predictions), i.e. test error
mean(lda.class != test$crim01)


## --- logistic regression
glm.fit = glm(crim01 ~ zn + indus + nox + rm +  rad,
              data = train, family = binomial)

glm.probs = predict(glm.fit, test, type = "response" )

## --- Change results from Prob. Vals. to Up/Down ---
glm.pred <- ifelse(glm.probs > 0.5, 1, 0)

## --- Confusion Matrix ---
table(glm.pred , test$crim01)

## --- Fraction of correct predictions for mpg01
mean(glm.pred == test$crim01)

## --- error rate (fraction of incorrect predictions), i.e. test error
mean(glm.pred != test$crim01)


## --- KNN
library(class)
train.X = cbind(train$zn, train$indus, train$nox, train$rm, train$rad )
test.X  = cbind(test$zn, test$indus, test$nox, test$rm, test$rad)
train.Direction = train$crim01

set.seed(1)

K = 5
knn.pred = knn(train.X, test.X, cl = train.Direction, k = K)
table(knn.pred, test$crim01)

## --- fraction of correct predictions
mean(knn.pred == test$crim01)

## --- fraction of incorrect predictions, i.e. error rate
mean(knn.pred != test$crim01)

