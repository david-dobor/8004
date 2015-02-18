library(MASS)
View(Boston)

names(Boston)
attach(Boston)
lm.fit = lm(medv~lstat)
lm.fit
summary(lm.fit)
names(lm.fit)
resid(lm.fit)
