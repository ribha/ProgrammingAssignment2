##This file contains two functions, one that takes a matrix object and returns a list of 4 functions to get/set the matrix and its inverse that can be cached
##The second function is to retrieve the cached inverse (if it exists)

## This function creates a special "matrix" object that can cache its inverse.
## It will return a list containing 4 functions; get and set for the matrix, & get and set for the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  inv <- solve(x$get())
  x$setinverse(inv)
  inv
}