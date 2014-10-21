## makeCacheMatrix and cacheSolve are a pair of functions used to compute and store 
## a matrix and its inverse into cache.
##
## makeCacheMatrix is a function that creates an object containing a matrix, its
## inverse and accompanying methods used to update and retrieve them.
##
## Usage:
## makeCacheMatrix(X) - stores the matrix X into an object that allows 
##    caching of its inverse
## This object can be updated/retrieved using the following methods:
##   $set(X) - overrides the previous matrix with X
##   $get()  - returns the current matrix
##   $setinverse(X) - sets the inverse of the matrix to X
##   $getInverse()  - returns the value of the inverse
## 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(M) inv <<- M
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## cacheSolve is a function that returns the inverse of a given matrix. If the inverse
## is already in the cache, it will simply return that inverse. Otherwise, it will 
## compute for the inverse and store it into the cache for future reference.
##
## Usage:
## cacheSolve(X) - Returns the inverse of matrix X. X must be an object created 
## by makeCacheMatrix.
##

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}



