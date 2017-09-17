## ------------------------------------------------------------------------------
## Created By: Kaushik Basu
## Created On: 16-Sep-2017
## Description: Coursera assignment for Week 3 
## GitHub Repository: https://github.com/thekaushikbasu/ProgrammingAssignment2 
## ==============================================================================

# Matrix inversion is usually a costly computation and there may be some benefit 
# to caching the inverse of a matrix rather than compute it repeatedly (there are 
# also alternatives to matrix inversion that we will not discuss here). 

# The objective of this assignment is to write a pair of functions that cache the inverse of a matrix.
# Computing the inverse of a square matrix can be done with the solve function in R. 
# For example, if X is a square invertible matrix, then solve(X) returns its inverse.

# For this assignment, it is assumed that the matrix supplied is always invertible.

## Function Name: makeCacheMatrix 
## Purpose: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(varArg1 = matrix()) {
  varNum <- NULL
  set <- function(varArg2) {
    varArg1 <<- varArg2
    varNum  <<- NULL
  }
  get <- function() varArg1
  setinverse <- function(inverse) varNum  <<- inverse
  getinverse <- function() varNum
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## function Name: cacheSolve
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache. 

cacheSolve <- function(varArg1, ...) {
  varNum <- varArg1$getinverse()
  if (!is.null(varNum)) {
    message("Getting Cached Data...")
    return(varNum)
  }
  data <- varArg1$get()
  varNum <- solve(data, ...)
  varArg1$setinverse(varNum)
  varNum ## Return a matrix that is the inverse of varArg1
}