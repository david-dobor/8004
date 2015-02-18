# consider the galapagos data in the faraway package
require(faraway)
data(gala)
head(gala[,-2]) #omit second column - will not use it

attach(gala)
lmod <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent)
summary(lmod)
sumary(lmod)
xtable(sumary(lmod))
# library(xtable)
# xtable(lmod)


#-------------------------------------------#
# directly computing statistics of interest #
#-------------------------------------------#

# Extract the X-matrix
x <- model.matrix( ~ Area + Elevation + Nearest + Scruz + Adjacent)
y <- gala$Species
xtxi <- solve(t(x) %*% x)
xtxi %*% t(x) %*% y
