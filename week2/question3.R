A <- matrix(c(4 , 4.001 , 4.001, 4.002), 2)
B <- matrix(c(4 , 4.001 , 4.001, 4.002001), 2)

det(A)  # nearly zer0
det(B)  # nearly zer0

Ainv <- solve(A)  
Binv <- solve(B)  

Binv

3 * Binv # check that approx this is Ainv
