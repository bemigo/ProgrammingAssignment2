## Functions that calculate the inverse of a given matrix x. 
## The result is cached to speed up calculations if the input matrix
## does not change.

## Usage:
## > x=matrix(); %Put your matrix here
## > A<-makeCacheMatrix(x); 
## > cacheSolve(A);

## makeCacheMatrix can be used to create a special "matrix" object.
## The only argument is a square invertible matrix x.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setSolve <- function(res) i <<- res
  getSolve <- function() i
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## cacheSolve can be used to compute the inverse of a special
## "matrix" object. This object is returned by the makeCacheMatrix
## function above.
## If the inverse has already been calculated, we get the cached result.
## Otherwise we compute the inverse and catch it.

cacheSolve <- function(x, ...) {
  i <- x$getSolve()
  if(!is.null(i)) {
    message("Getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setSolve(i)
  
  ## Return a matrix that is the inverse of 'x'  
  i
}
