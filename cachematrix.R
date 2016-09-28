## Peer-graded Assignment: Programming Assignment 2
## Lexical Scoping


## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse matrix
## get the value of the inverse matrix

makeCacheMatrix <- function(a = matrix()) {
  b <- NULL
  set <- function(y) {
    a <<- y
    b <<- NULL
  }
  get <- function() a
  setinverse <- function(inv) b <<- inv
  getinverse <- function() b
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(a, ...) {
  b <- a$getinverse()
  if(!is.null(b)) {
    message("getting cached data")
    return(b)
  }
  data <- a$get()
  b <- solve(data, ...)
  a$setinverse(b)
  b
}
