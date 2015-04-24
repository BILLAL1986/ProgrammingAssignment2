## This function describes how a pair of functions cache the inverse of a matrix.

## makeCacheMatrix is the principal function recevies a matrix as an input
## and it contains 4 functions
## get, set, setinverse and getinverse



makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
  
}
## cacheSolve is a function calculates the inverse of the matrix
## and sets the inverse matrix in the cache data

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
## Return a matrix that is the inverse of 'x'
