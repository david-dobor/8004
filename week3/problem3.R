# question 3
# will need generalized inverse (ginv() function) in the MASS library:
library(MASS)

# data given for problem 2:
X <- matrix( 
    c(1, 1, 1, 1, 1, 1, 
      1, 1, 0, 0, 0, 0,
      0, 0, 1, 0, 0, 0,
      0, 0, 0, 1, 0, 0, 
      0, 0, 0, 0, 1, 1 ), 
    nrow=6, 
    ncol=5) 

y <- matrix(c(2, 1, 4, 6, 3, 5), 6, 1)

lm(y ~ X[,2:5] -1, weights=c(1, 1/9, 1/9, 1, 1, 1/9))

S <- diag(c(1, 1/3, 1/3, 1, 1, 1/3))

y1 <- S%*%y
x1 <- S%*%X
lm(y1 ~ x1)

