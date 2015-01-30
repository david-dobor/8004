# Solution to question 2

# V, the given matrix:
V <- matrix(c(3, -1, 1, -1, 5, -1, 1, -1, 3), 3) 

# Q (orthogonal here) contains eigenvectors in its columns: 
Q <- eigen(V)$vectors              

# D contains the inverse square roots of of V's eigenvalues 
# on the diagonal, zeros elsewhere:
D <- diag(1/sqrt(eigen(V)$values))

# The desired `inverse square root` matrix:
W <- Q %*% D %*% t(Q)      



# W %*% W #check that this returns solve(V)
