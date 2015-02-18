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

qr(X)$rank

#------------------------- part (a) -------------------------
# compute sum of squared errors
beta.hat <- ginv(t(X) %*% X) %*% t(X) %*% Y
Y.hat <- X %*% beta.hat;  
SSE <- t(Y - Y.hat) %*% (Y - Y.hat) # ans: 2.5

# compute the endpoints for the 90% confidence interval
lower.limit <- SSE / qchisq(0.95, 2)  # ans: 0.4172603
upper.limit <- SSE / qchisq(0.05, 2)  # ans: 24.36966

c(sqrt(lower.limit), sqrt(upper.limit))
#ans: 0.6459568 4.9365633