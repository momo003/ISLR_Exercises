set.seed(1)
library(ISLR)
library(boot)
attach(Default)

##    a
glm.fit = glm(default ~ income + balance + student,
              data = Default, family = binomial)

summary(glm.fit)$coef


##    b
boot.fn = function(data, index){
  glmfit = glm(default ~ income + balance + student,
                data = data, family = binomial,
                subset = index)
  return(coef(glmfit))
}

## test function
trainid = sample(1:nrow(Default), nrow(Default)*0.7, replace = F)
coff = boot.fn(Default, trainid)

##    c
boot(Default, boot.fn, R=1000)
