# question 2
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

c.t <- matrix( 
    c(1, 1, 1, 1, 
      1, 0, 0, 0,
      0, 1, 0, 0,
      0, 0, 1, 0,
      0, 0, 0, 1), 
    nrow=4, 
    ncol=5) 

#----------------------------------------------------------
# part (a)
#----------------------------------------------------------

# error variance 
V1 <- diag(c(1, 9, 9, 1, 1, 9));
V1.inv <- solve(V1);
# transformation to convert Aiken to Gauss-Markov:
V1.inv.sqrt = sqrt(V1.inv);

# transform data with V1.inv.sqrt:
u <- V1.inv.sqrt %*% y
W <- V1.inv.sqrt %*% X

# beta.hat in the transformed model:
beta.hat <- ginv(t(W) %*% W) %*% t(W) %*% u

# compute u.hat
u.hat <- W %*% beta.hat

# back to y.hat
y.hat <- solve(V1.inv.sqrt) %*% u.hat

## The estimate of c^T \beta
c.t %*% ginv(t(W) %*% W) %*% t(W) %*% u


#----------------------------------------------------------
# part (b)
#----------------------------------------------------------
V2 <- matrix( 
    c(1,  1,   0,   0,  0,  0,
      1,  9,   0,   0,  0,  0,
      0,  0,   9,  -1,  0,  0,
      0,  0,  -1,   1,  0,  0,
      0,  0,   0,   0,  1, -1,
      0,  0,   0,   0, -1,  9),
    nrow=6, 
    ncol=6) 

# first, compute the inverse square root of V2
Q <- eigen(V2)$vectors              
# D contains the inverse square roots of of V's eigenvalues 
# on the diagonal, zeros elsewhere:
D <- diag(1/sqrt(eigen(V2)$values))

# The desired `inverse square root` matrix.
# It's the transformation to convert Aiken to Gauss-Markov:
V2.inv.sqrt <- Q %*% D %*% t(Q)  

# transform data with V2.inv.sqrt:
u <- V2.inv.sqrt %*% y
W <- V2.inv.sqrt %*% X

# beta_hat in the transformed model:
beta.hat <- ginv(t(W) %*% W) %*% t(W) %*% u

# compute u.hat
u.hat <- W %*% beta.hat

# back to y.hat
y.hat <- solve(V2.inv.sqrt)%*% u.hat

## The estimate of c^T \beta
c.t %*% ginv(t(W) %*% W) %*% t(W) %*% u

#----------------------------------------------------------
# scratch
#----------------------------------------------------------

# source("project.R")
# y.hat <- project(X) %*% y
