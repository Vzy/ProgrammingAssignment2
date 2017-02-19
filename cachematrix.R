## Put comments here that give an overall description of what your
## functions do

# This function caches a matrix and its inverse in a list

makeCacheMatrix <- function(x=matrix()) {
  inv <- NULL
  set <- function(y=matrix()) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(solve) inv <<- solve
  getInv <- function() inv
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}

## This function checks in the resulting list from the previous funciton is the inverse matrix is there, otherwise it calculates the inverse
          
SolveCache <- function(x, ...) {
    inv <- x$getInv()
    if (!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInv(inv)
    return(inv)
  }
