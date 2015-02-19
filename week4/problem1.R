# question 1
library(MASS)

# data given for problem 2, previous assignment:
X <- matrix( 
    c(1, 1, 1, 1, 1, 1, 
      1, 1, 0, 0, 0, 0,
      0, 0, 1, 0, 0, 0,
      0, 0, 0, 1, 0, 0, 
      0, 0, 0, 0, 1, 1 ), 
    nrow=6, 
    ncol=5) 

Y <- matrix(c(2, 1, 4, 6, 3, 5), 6, 1)


# compute some useful quantities used later
XtXi <- ginv(t(X) %*% X) 
n = 6 
df = n - qr(X)$rank  #degrees of freedom (used in tests below)
beta.hat <- XtXi %*% t(X) %*% Y
Y.hat <- X %*% beta.hat; 
SSE <- t(Y - Y.hat) %*% (Y - Y.hat) # ans: 2.5


#------------------------- part (a) -------------------------
# compute the endpoints for the 90% confidence interval
lower.limit <- SSE / qchisq(0.95, 2)  # ans: 0.4172603
upper.limit <- SSE / qchisq(0.05, 2)  # ans: 24.36966

c(sqrt(lower.limit), sqrt(upper.limit))
#ans: 0.6459568 4.9365633


#------------------------- part (b) -------------------------
c <- matrix(c(1, 0, 1, 0, 0), 5, 1)
c.beta.hat <- t(c) %*% beta.hat #= 4
MSE <- SSE / df

# 90% two sided confidence interval
c.beta.hat +
    c(-1, 1) * qt(.95, df)* sqrt(MSE) * sqrt(t(c) %*% XtXi %*% c)
#ans: 0.7353569 7.2646431


#------------------------- part (c) -------------------------
c <- matrix(c(0, 1, -1, 0, 0), 5, 1)
c.beta.hat <- t(c) %*% beta.hat #= -2.5

# 90% two sided confidence interval
c.beta.hat +
    c(-1, 1) * qt(.95, df) * sqrt(MSE) * sqrt(t(c) %*% XtXi %*% c)
#ans: -3.998355 3.998355


#------------------------- part (d) -------------------------
# here c and c.beta.hat are the same as in part (c)

# sum of squares under the null (numerator in the F test):
SSH <- t(c.beta.hat) %*% ginv( (t(c) %*% XtXi %*% c) ) %*% c.beta.hat

SSE <- t(Y - Y.hat) %*% (Y - Y.hat)
MSH <- SSH
MSE <- SSE / df

# the F ratio
F <- MSH / MSE
# the p-value
1 - pf(F, 1, 2) #ans: 0.2094306


#------------------------- part (e) -------------------------
c <- matrix(c(1, 1, 0, 0, 0), 5, 1)
c.beta.hat <- t(c) %*% beta.hat #= 1.5
gamma <- 1/10
MSE <- SSE / df

c.beta.hat + c(-1,1) * qt(.95, df) * sqrt(MSE) *sqrt(gamma + t(c) %*% XtXi %*% c)
# ans: -1.028782  4.028782


#------------------------- part (f) -------------------------
c <- matrix(c(2, 1, 1, 0, 0), 5, 1)
c.beta.hat <- t(c) %*% beta.hat #= 5.5
gamma <- 2
MSE <- SSE / df

c.beta.hat + c(-1,1) * qt(.95, df) * sqrt(MSE) *sqrt(gamma + t(c) %*% XtXi %*% c)
# ans: -0.607588 11.607588

#------------------------- part (g) -------------------------
C <- t(matrix(c(0, 1, -1, 0, 0,
                0, 1, 0, -1, 0,
                0, 1, 0, 0, -1), nrow=5, ncol=3))
C.beta.hat <- C %*% beta.hat
# sum of squares under the null (numerator in the F test):
SSH <-
    t(C.beta.hat) %*% ginv( (C %*% XtXi %*% t(C)) ) %*% C.beta.hat
SSE <- t(Y - Y.hat) %*% (Y - Y.hat)
MSH <- SSH
MSE <- SSE / df
# the F ratio
F <- MSH / MSE
1 - pf(F, 1, 2) #ans: 0.0741799



#------------------------- part (h) -------------------------
C <- t(matrix(c(0, 1, -1, 0, 0,
                0, 0, 1, -1, 0), nrow=5, ncol=2))
d <- matrix(c(10,0))
u <- C %*% beta.hat - d
# sum of squares under the null (numerator in the F test):
SSH <-
    t(u) %*% ginv( (C %*% XtXi %*% t(C)) ) %*% u
SSE <- t(Y - Y.hat) %*% (Y - Y.hat)
MSH <- SSH
MSE <- SSE / df
# the F ratio
F <- MSH / MSE
1 - pf(F, 1, 2) #ans: 0.006715993
