## This function describes how a pair of functions cache the inverse of a matrix.

## makeCacheMatrix is the principal function recevies a matrix as an input
## and it contains 4 functions
## get, set, setinverse and getinverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  ## the set function changes the vector stored in the main function
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  ## get function returns the vector x stored in the main function
  get <- function() x
  ## Inversing the matrix using solve() 
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
 ## Check if we have a matrix in the cache
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  ## no matrix in the cache
  ## in this case we create an inverted matrix
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
## Return a matrix that is the inverse of 'x'
