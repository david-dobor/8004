# This script helps answer questions for problem 5, assignment 2.
#
# To test whether a vector belongs to the rowspace of a design matrix,
# project it onto the space (Equivalently, project it onto the column
# space of X^T). If the projection is the same as the vector being 
# projected, then it's in the space; otherwise it's not.

# The design matrix
X = matrix( 
    c(1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 0, 0, 0, 0,
      0, 0, 0, 0, 1, 1, 1, 1,
      1, 1, 0, 0, 1, 1, 0, 0,
      0, 0, 1, 1, 0, 0, 1, 1,
      1, 1, 0, 0, 0, 0, 0, 0,
      0, 0, 1, 1, 0, 0, 0, 0,
      0, 0, 0, 0, 1, 1, 0, 0,
      0, 0, 0, 0, 0, 0, 1, 1), 
    nrow=8, 
    ncol=9) 

# load the project() function from problem 4:
source("project.R")
P <- project(t(X))

#------------------------------------------------
# question: is alpha1 estimable?
alpha1 <- matrix(c(0, 1, 0, 0, 0, 0, 0, 0, 0))
print(P %*% alpha1)        #not estimable


#------------------------------------------------
# question: is alpha2 - alpha1 estimable:
alpha2_1 <- matrix(c(0, -1, 1, 0, 0, 0, 0, 0, 0))
print(P %*% alpha2_1)      # not estimable 


#------------------------------------------------
mu_and_others <- matrix(c(1, 1, 0, 1, 0, 1, 0, 0, 0))
print(P %*% mu_others)     # estimable

#------------------------------------------------
delta1_2 <- matrix(c(0, 0, 0, 0, 0, 0, 1, 0, 0))
P %*% delta1_2

#------------------------------------------------
deltas  <- matrix(c(0, 0, 0, 0, 0, -1, 1, 1, -1))
P %*% deltas               # estimable 


