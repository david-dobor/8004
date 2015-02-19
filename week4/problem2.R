# assignment 4, problem 2
library(MASS)
data(Boston)

Y=as.matrix(Boston$medv)
X=as.matrix(Boston[,c('crim','nox','rm','age','dis')])
X=cbind(rep(1,dim(Boston)[1]),X)

# some preliminary quantities useful in what follows:
XtXi <- ginv(t(X) %*% X) 
n <- dim(X)[1]         # 506 observations
df <- n - qr(X)$rank   # 500 degrees of freedom
beta.hat <- XtXi %*% t(X) %*% Y
Y.hat <- X %*% beta.hat; 
SSE <- t(Y - Y.hat) %*% (Y - Y.hat)  # ans: 17411.94
MSE <- SSE / df                      # ans: 34.82387

#------------------------- part (a) -------------------------
# compute the endpoints for the 90% confidence interval
lower.limit <- SSE / qchisq(0.95, 500)  # ans: 31.4791
upper.limit <- SSE / qchisq(0.05, 500)  # ans: 38.7667

c(sqrt(lower.limit), sqrt(upper.limit))
# ans: 5.610624 6.226291

#------------------------- part (b) -------------------------
c <- X[1,]   # data point # 1: first row
c.beta.hat <- t(c) %*% beta.hat # ans: 25.70437

# 90% two sided confidence interval
c.beta.hat +
    c(-1, 1) * qt(.95, df)* sqrt(MSE) * sqrt(t(c) %*% XtXi %*% c)

# ans: 25.21142 26.19733


#------------------------- part (c) -------------------------
c <- X[1,] - X[2,]   # data point # 1: first row - second row
c.beta.hat <- t(c) %*% beta.hat #

# 90% two sided confidence interval
c.beta.hat +
    c(-1, 1) * qt(.95, df)* sqrt(MSE) * sqrt(t(c) %*% XtXi %*% c)

# ans: 1.202479 2.612541


#------------------------- part (d) -------------------------
# here c and c.beta.hat are the same as in part (c)
c <- X[1,] - X[2,]   # data point # 1: first row - second row
c.beta.hat <- t(c) %*% beta.hat 

# two sided t-test
p.value <- 2*(1 - pt(abs(c.beta.hat / (sqrt(MSE) * sqrt(t(c) %*% XtXi %*% c))),df))
# ans: 1.019758e-05


#------------------------- part (e) -------------------------
c <- c(1, 0.005, 0.45, 7, 45, 6)
se <- sqrt(MSE)*sqrt(1 + c %*% XtXi %*% c)   #ans: 5.917394
c.beta.hat <- t(c) %*% beta.hat 

c.beta.hat + c(-1, 1) * qt(.95, df) * se
# ans: 19.90023 39.40286





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
MSE <- SSE / df

# the F-ratio and the p-value
F <- (SSH / 2) / MSE
1 - pf(F, 2, 500)   #ans: 3.190781e-13


