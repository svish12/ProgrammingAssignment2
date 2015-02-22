## Programming Assignment 2: Lexical Scoping
##
## In this example we introduce the <<- operator which can be used to assign 
## a value to an object in an environment that is different from the current 
## environment. Below are two functions that are used to create a special object 
## that stores a numeric matrix and cache's its inverse.

## The "makeCacheMatrix" function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## The "cacheSolve" function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  if (nrow(x$get()) != ncol(x$get())) {
    message("No possible to invert a rectangular matrix")
    return()
  }
  ## Return a matrix that is the inverse of x
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)    
  x$setinv(i)
  i
}
