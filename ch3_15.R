

## predict the per capita crime rate 

setwd('~/Documents/R/ISLR')
library(MASS)

##  a     (perform multiple fit using a lapply to avoid repeatation)
dvList = names(Boston)[2:14]

# model = sapply(dvList, 
#         function(x){lm(substitute(crim~i,
#                     list(i = as.name(x))), data = Boston)},
#         simplify = "array")
# sapply(model, summary, simplify = "array")

lm.fit.zn = lm(crim ~ zn , data = Boston)
coffs = lm.fit.zn$coefficients[2]

lm.fit.indus = lm(crim ~ indus , data = Boston)
coffs = c(coffs, lm.fit.indus$coefficients[2])

lm.fit.chas = lm(crim ~ chas , data = Boston)
coffs = c(coffs, lm.fit.chas$coefficients[2])

lm.fit.nox = lm(crim ~ nox , data = Boston)
coffs = c(coffs, lm.fit.nox$coefficients[2])

lm.fit.rm = lm(crim ~ rm , data = Boston)
coffs = c(coffs, lm.fit.rm$coefficients[2])

lm.fit.age = lm(crim ~ age , data = Boston)
coffs = c(coffs, lm.fit.age$coefficients[2])

lm.fit.dis = lm(crim ~ dis , data = Boston)
coffs = c(coffs, lm.fit.dis$coefficients[2])

lm.fit.rad = lm(crim ~ rad , data = Boston)
coffs = c(coffs, lm.fit.rad$coefficients[2])

lm.fit.tax = lm(crim ~ tax , data = Boston)
coffs = c(coffs, lm.fit.tax$coefficients[2])

lm.fit.ptratio = lm(crim ~ ptratio , data = Boston)
coffs = c(coffs, lm.fit.ptratio$coefficients[2])

lm.fit.black = lm(crim ~ black , data = Boston)
coffs = c(coffs, lm.fit.black$coefficients[2])

lm.fit.lstat = lm(crim ~ lstat , data = Boston)
coffs = c(coffs, lm.fit.lstat$coefficients[2])

lm.fit.medv = lm(crim ~ medv , data = Boston)
coffs = c(coffs, lm.fit.medv$coefficients[2])

## b
lm.fit.all = lm(crim ~ . , data = Boston)
summary(lm.fit.all)


##  c
coffs.a = coffs
coffs.b = lm.fit.all$coefficients[2:14]
pdf('ch3_15.pdf')
plot(coffs.a , coffs.b)
dev.off()

## d
model = sapply(dvList, 
         function(x){lm(substitute(crim~i+I(i^2)+I(i^3),
                      list(i = as.name(x))), data = Boston)},
         simplify = "array")
sapply(model, summary, simplify = "array")
 
 