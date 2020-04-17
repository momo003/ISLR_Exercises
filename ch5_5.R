library(ISLR)

set.seed(1)
attach(Default)

## ---  a
glm.fit = glm(default ~ income + balance, 
              data = Default, family = binomial)
glm.probs = predict(glm.fit, type = "response")
contrasts(default)
glm.pred = ifelse(glm.probs > 0.5, "Yes", "No")

## confusion table
table(glm.pred, default)

## error
mean(glm.pred != default)    

## ---  b & c

for (i in 1:3) {
  print(i)
  set.seed(i)
  trainid = sample(1:nrow(Default), nrow(Default)*0.7, replace = F)
  train = Default[trainid,]
  test  = Default[-trainid,]
  
  glm.fit = glm(default ~ income + balance,
                data = train, family = binomial)
  
  glm.probs = predict(glm.fit, test, type = "response")
  glm.pred  = ifelse(glm.probs > 0.5, "Yes", "No")
  
  ## confusion table
  table(glm.pred, test$default)
  
  ## error in prediction on validation
  error = mean(glm.pred != test$default)
  print(error )
}


## ---  d
set.seed(1)
trainid = sample(1:nrow(Default), nrow(Default)*0.7, replace = F)
train = Default[trainid,]
test  = Default[-trainid,]

glm.fit = glm(default ~ income + balance + student,
              data = train, family = binomial)
contrasts(student)
glm.probs = predict(glm.fit, test, type = "response")
glm.pred  = ifelse(glm.probs > 0.5, "Yes", "No")

## confusion table
table(glm.pred, test$default)

## error in prediction on validation
error = mean(glm.pred != test$default)
print(error )


