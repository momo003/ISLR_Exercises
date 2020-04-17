lm.fit.zn = lm(crim ~ zn, data = Boston)
summary(lm.fit.zn)

# coef(lm.fit.zn)
# coef(summary(lm.fit.zn))[, "t value"]
# coef(summary(lm.fit.zn))[, 3]
# coef(summary(lm.fit.zn))[, 4]

#   ## '=================================='
# lm.fit.indus = lm(crim ~ indus, data = Boston)
# summary(lm.fit.indus)
#   ## '=================================='
# lm.fit.chas = lm(crim ~ chas, data = Boston)
# summary(lm.fit.chas)
#   ## '=================================='
# lm.fit.nox = lm(crim ~ nox, data = Boston)
# summary(lm.fit.nox)
#   ## '=================================='
# lm.fit.rm = lm(crim ~rm, data = Boston)
# summary(lm.fit.rm)
#   ## '=================================='
# lm.fit.age = lm(crim ~ age, data = Boston)
# summary(lm.fit.age)
#   ## '=================================='
# lm.fit.dis = lm(crim ~ dis, data = Boston)
# summary(lm.fit.dis)
#   ## '=================================='
# lm.fit.rad = lm(crim ~ rad, data = Boston)
# summary(lm.fit.rad)
#   ## '=================================='
