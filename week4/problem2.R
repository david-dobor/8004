# assignment 4, problem 2
library(MASS)
data(Boston)

Y=as.matrix(Boston$medv)
X=as.matrix(Boston[,c('crim','nox','rm','age','dis')])
X=cbind(rep(1,dim(Boston)[1]),X)

# some preliminary checks. e.g., rank(X), etc.
x <- model.matrix(~ crim + nox + rm + age + dis)
qr(X)$rank

#------------------------- part (a) -------------------------
# compute sum of squared errors
beta.hat <- ginv(t(X) %*% X) %*% t(X) %*% Y
Y.hat <- X %*% beta.hat;  
SSE <- t(Y - Y.hat) %*% (Y - Y.hat) # ans: 17411.94

# compute the endpoints for the 90% confidence interval
lower.limit <- SSE / qchisq(0.95, 500)  # ans: 31.4791
upper.limit <- SSE / qchisq(0.05, 500)  # ans: 38.7667


#------------------------- part (b) -------------------------
#

#------------------------- part (f) -------------------------
C <- matrix( 
    c(0, 0, 
      0, 0, 
      1, 0,
      0, 0, 
      0, 1,
      0, 0),
    nrow=2, 
    ncol=6) 

XtXi <- ginv(t(X) %*% X); 
C.beta.hat <- C %*% beta.hat

# sum of squares under the null (numerator in the F test):
SSH <- 
    t(C.beta.hat) %*% ginv( (C %*% XtXi %*% t(C)) )  %*% C.beta.hat

# squared errors (same as before, part (a) denominator in the F test):
SSE <- t(Y - Y.hat) %*% (Y - Y.hat) # ans: 17411.94
sigma.squared <- SSE / 500

# non cenrality parameter for F
ncp <- (1/2) * (1 / sigma.squared) * SSH

# the F-ratio and the p-value
F <- (SSH / 2) / (SSE / 500)
1 - pf(F, 2, 500, ncp)   #ans: 0.01607116
