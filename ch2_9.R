# load  Auto dataset from library
library(ISLR)
data(Auto)
summary(Auto)
nrow(Auto)

range(Auto$mpg)
mean(Auto$mpg)
sd(Auto$mpg)

# cut out obs. 10 to 85 
cutauto = Auto[-seq(10,85),]
nrow(cutauto)

range(cutauto$mpg)
mean(cutauto$mpg)
sd(cutauto$mpg)

pairs(Auto[,1:7])
