## Functions to retrieve the inverse of a matrix if one exists
## and compute it if there is no cached value.

## Creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  matInverse <- NULL
  
  set <- function(y) {
    x <<- y
    matInverse <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(value) matInverse <<- value
  
  getInverse <- function() matInverse
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Tries to retrieve a cached value of the inverse before solving

cacheInverse <- function(x, ...) {

  matInverse <- x$getInverse()
  
  if(!is.null(matInverse)) {
    message("getting cached data")
    return(matInverse)
  }
  
  data <- x$get()
  
  matInverse <- solve(data, ...)
  
  x$setInverse(matInverse)
  
  matInverse
}
