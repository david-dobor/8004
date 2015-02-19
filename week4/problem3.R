# question 3
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
C <- t(matrix(c(0, 1, -1, 0, 0,
                0, 1, 0, -1, 0,
                0, 1, 0, 0, -1), nrow=5, ncol=3))
C.beta.hat <- C %*% beta.hat
# sum of squares under the null (numerator in the F test):
SSH <-
    t(C.beta.hat) %*% ginv( (C %*% XtXi %*% t(C)) ) %*% C.beta.hat


middle <- ginv( (C %*% XtXi %*% t(C)) )
C.beta.hat - c(0, 0, 1)

MSH <- SSH / 3

SSE <- t(Y - Y.hat) %*% (Y - Y.hat)
MSE <- SSE / df
# the F ratio
F <- MSH / MSE
1 - pf(F, 1, 2) #ans: 0.1835034


# compute the non-centrality parameter
ginv( (C %*% XtXi %*% t(C)) )

XtXi %*% t(C)



#------------------------- part (b) -------------------------