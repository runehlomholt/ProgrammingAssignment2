## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: create a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL

  set <- function(y) {
    if (!is.matrix(y)) stop("Input must be a matrix")
    x <<- y
    inv <<- NULL                 # reset cached inverse when matrix changes
  }

  get <- function() x

  setinverse <- function(i) inv <<- i
  getinverse <- function() inv

  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve: compute (or retrieve cached) inverse of the special matrix
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }

  mat <- x$get()

  inv <- solve(mat, ...)     # compute inverse
  x$setinverse(inv)          # cache it
  inv
}
