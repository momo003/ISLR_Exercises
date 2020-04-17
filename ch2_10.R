library(MASS)
data(Boston)
summary(Boston)
nrow(Boston)
ncol(Boston)
#fix(Boston)
names(Boston)


#pairs(Boston[,c(1,seq(9,14))])
#pairs(Boston)
max(Boston$crim)
which.max(Boston$crim)
mean(Boston$crim)

max(Boston$tax)
which.max(Boston$tax)
mean(Boston$tax)

max(Boston$ptratio)
which.max(Boston$ptratio)
mean(Boston$ptratio)

range(Boston$crim)
range(Boston$tax)
range(Boston$ptratio)
