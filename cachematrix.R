## The functions below provide a framework for calculating 
## inverses of matricies with caching the calculated results.
## In order to use the functionality a matrix should be wrapped 
## with makeCacheMatrix functions and further submitted 
## to cacheSolve function.

## Creates a wrapper over a function which enables supplying a matrix to cacheSolve function.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  get <- function() x
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getInverse <- function() inv
  setInverse <- function(y){ inv <<- y}
  list(get = get, set = set, getInverse = getInverse, setInverse = setInverse)
}


## Calculates the inverse of a matrix wrapped with makeCacheMatrix function. The calculated result is cached
## in order to avoid repeated recalculation.
cacheSolve <- function(x, ...) {
  ## Retrieve cached inverse value
  inv <- x$getInverse()
  if(is.null(inv)) {
    ## If NULL, calculate and add to cache
    inv <- solve(x$get(), ...)
    x$setInverse(inv)
  }
  inv
}
