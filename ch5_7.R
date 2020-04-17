library(ISLR)
attach(Weekly)
set.seed(1)

##    a
glm.fit = glm(Direction ~ Lag1 + Lag2, family = binomial)
summary(glm.fit)$coef

glm.probs = predict.glm(glm.fit, type= "response")
glm.pred = ifelse(glm.probs > 0.5 , "Up", "Down")
table(glm.pred, Direction)

##    b
set.seed(1)

glm.fit = glm(Direction ~ Lag1 + Lag2, family = binomial, 
              data = Weekly[-1,])

##    c
glm.probs = predict.glm(glm.fit, Weekly[1,], type = "response")
glm.pred = ifelse(glm.probs > 0.5, "Up", "Down")

##  actual
print(Weekly$Direction[1])

##   predictions
print(glm.pred)


##    d
loocv.err = rep(0,nrow(Weekly))
n = dim(Weekly)[1]
for (i in 1:n) {
  glm.fit = glm(Direction ~ Lag1 + Lag2, family = binomial, 
                data = Weekly[-i,])
  glm.probs = predict.glm(glm.fit, Weekly[i,], type = "response")
  glm.pred = ifelse(glm.probs > 0.5, "Up", "Down")
  loocv.err[i] = ifelse(Weekly[i,]$Direction == glm.pred, 0, 1)
}

##  show structure of loocv.err
str(loocv.err)

##    e
mean(loocv.err)
