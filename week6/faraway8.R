# Chapter 8 of Extending the Linear Model with R, Faraway

library(faraway)
data(pulp)

op <- options(contrasts=c("contr.sum", "contr.poly"))
lmod <- aov(bright ~ operator, pulp)
summary(lmod)
coef(lmod)
model.matrix(lmod)

library(lme4)
#
mmod <- lmer(bright ~ 1 + (1|operator), pulp)
summary(mmod)

# mle estimates may also be computed
smod <- lmer(bright ~ 1 + (1|operator), pulp, method="ML")
summary(smod)


############# Penicillin Data 
data(penicillin)
summary(penicillin)
op <- options(contrasts=c("contr.sum", "contr.poly"))
lmod <- aov(yield ~ blend + treat, penicillin)
summary(lmod)
coef(lmod)
