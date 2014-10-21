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
  inv <- NULL               ## placeholder for the inverse of matrix x
  set <- function(y) {      ## set function accepts a matrix y and stores into cache
    x <<- y                 ## any previous values of the inverse is erased
    inv <<- NULL
  }
  get <- function() x       ## get function returns the value of the matrix stored in cache
  setInverse <- function(M) {  ## setInverse function sets the value of the inverse matrix
    inv <<- M
  } 
  getInverse <- function() inv  ## getInverse function returns the cached value of inverse
  
  list(set = set, get = get,           ## calling makeCacheMatrix returns a
       setInverse = setInverse,        ## list of methods to access cached matrices
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

  m <- x$getInverse()               ## retrieve inverse stored in cache
  if(!is.null(m)) {                 ## if inverse is in cache, return that matrix
    message("getting cached data")
    return(m)
  }
  data <- x$get()                   ## retrieve the matrix
  m <- solve(data, ...)             ## compute for the inverse
  x$setInverse(m)                   ## store the inverse in cache
  m                                 ## return the computed inverse matrix
}



