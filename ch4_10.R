setwd('~/Documents/R/ISLR')
library(ISLR)
attach(Weekly)

##      a
pairs(Weekly)
corr = cor(Weekly[,-9])
print(corr)

##      b     (logistic regression)

glm.fit = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
              data = Weekly, family = binomial)
summary(glm.fit)

print('P-value for all are relatively large. Smallest one is for Lag1 with
       a negative coefficient: that means if last week the market went up,
       it is unlikely this week also goes up')

##      c

contrasts(Direction)

## --- prediction
glm.probs = predict(glm.fit, type = "response")
glm.pred = rep("Down", length(Direction))
glm.pred[glm.probs > 0.5] = "Up"

## --- confusion matrix
table(glm.pred, Direction)

## --- Fraction of weeks for which prediction was correct
mean(glm.pred == Direction)
mean(glm.pred != Direction)


##      d
## --- separate train and test sets
train = (Weekly$Year < 2009)
Weekly.9.10 = Weekly[!train, ]
dim(Weekly.9.10)
Direction.9.10 = Weekly$Direction[!train]

## --- perfrom logit on train
glm.fit = glm(Direction ~ Lag2, data = Weekly,
              family = binomial, subset = train)

## --- predict for test
glm.probs = predict(glm.fit, Weekly.9.10, type = "response")
glm.pred  = rep("Down", length(glm.probs))
glm.pred[glm.probs > 0.5] = "Up"

## --- confusion matrix
table(glm.pred, Direction.9.10)

## --- fraction of correct predictions
mean(glm.pred == Direction.9.10)

## --- error rate (fraction of incorrect predictions)
mean(glm.pred != Direction.9.10)


##      e     (LDA)
library(MASS)

lda.fit = lda(Direction ~ Lag2, data = Weekly, subset = train)

lda.fit
plot(lda.fit)

lda.pred = predict(lda.fit, Weekly.9.10)
names(lda.pred)

lda.class = lda.pred$class

## --- Confusion matrix
table(lda.class , Direction.9.10)

## --- fracion of  weeks for which lda had correct predictions
mean(lda.class == Direction.9.10)
## --- error rate    (fraction of incorrect predictions)
mean(lda.class != Direction.9.10)

## --- Use posterior probs. (> 50 %) to re-create the results in lda.pred$class
##  (i.e. sum of rows in confusion matrix)
sum(lda.pred$posterior[,1] >= 0.5)
sum(lda.pred$posterior[,1] < 0.5)

## use other theresholds for probs., e.g. market decreases prob < 90%
sum(lda.pred$posterior[,1] >= 0.59)


##      f     (QDA)
##    train
qda.fit = qda(Direction ~ Lag2, data = Weekly, subset = train)
qda.fit

##   test
qda.pred = predict(qda.fit, Weekly.9.10)
qda.class = qda.pred$class

## --- Confusion matrix
table(qda.class, Direction.9.10)

## --- fraction of correct predictions
mean(qda.class == Direction.9.10)


##      g      (KNN , K = 1)
library(class)

## --- knn needs train and test as matrices, thus we use cbind
train.X = cbind(Weekly$Lag2[train])
test.X  = cbind(Weekly$Lag2[!train])
train.Direction = Weekly$Direction[train]



set.seed(1)

K = 1
knn.pred = knn(train.X, test.X, cl = train.Direction, k = K)

table(knn.pred, Direction.9.10)

## --- fraction of correct predictions
mean(knn.pred == Direction.9.10)

## --- error rate in predictions
mean(knn.pred != Direction.9.10)



##       i

## --- logit
glm.fit = glm(Direction ~ Lag2 + Volume + Year , data = Weekly,
              family = binomial, subset = train)
glm.probs = predict(glm.fit, Weekly.9.10, type = "response")
glm.pred  = rep("Down", length(glm.probs))
glm.pred[glm.probs > 0.5] = "Up"
table(glm.pred, Direction.9.10)

# --- KNN
train.X = cbind(Weekly$Year, Weekly$Volume, Weekly$Lag2)[train,]
test.X  = cbind(Weekly$Year, Weekly$Volume, Weekly$Lag2)[!train,]
train.Direction = Weekly$Direction[train]
K = 1
knn.pred = knn(train.X, test.X, cl = train.Direction, k = K)
table(knn.pred, Direction.9.10)
dev.off()

## --- LDA
lda.fit = lda(Direction ~  Volume + Year + Lag2, data = Weekly, subset = train)
lda.pred = predict(lda.fit, Weekly.9.10)
lda.class = lda.pred$class
table(lda.class , Direction.9.10)

## --- QDA
qda.fit = qda(Direction ~ Volume + Year + Lag2, data = Weekly, subset = train)
qda.fit
qda.pred = predict(qda.fit, Weekly.9.10)
qda.class = qda.pred$class
table(qda.class, Direction.9.10)
