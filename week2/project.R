# solution to question 4
project <- function(X) {
    # Computes the projection matrix onto the column space of X
    #
    # Args:
    #   X: a matrix
    #
    # Returns:
    #   P: the projection matrix
    
    # library MASS' ginv() computes the generalized inverse:
    suppressPackageStartupMessages(library(MASS)) 
    
    P <- X %*% ginv(t(X) %*% X) %*% t(X)
}