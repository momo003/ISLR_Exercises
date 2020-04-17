## we attempt to predict 'Sales'
#fix(Carseats)

attach(Carseats)
names(Carseats)

## multiple regression, including interaction terms (some predictors are qualitative)
lm.fit = lm(Sales~.+ Income:Advertising+Price:Age, data = Carseats)
summary(lm.fit)

## show/change the coding used by R for qualitative (ShelveLoc) predictor
contrasts(ShelveLoc)


