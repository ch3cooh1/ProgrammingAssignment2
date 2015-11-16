## The purpose of this file to create two functions.  The combined purpose of these is to solve for the inverse of
## a matrix and store that solution as a cached value.  This will serve to conserve resources.

## This function will create a list that will feed a variable used in the following function.

makeCacheMatrix <- function(x = matrix()) {
  invert <- NULL
  set <- function(y) {
    x <<- y
    invert <<- NULL
  }
  get <- function() x
  setinv <- function(solve) invert <<- solve
  getinv <- function() invert
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



## This function will generate the inverse of a matrix using values from makeCacheMatrix.  It will first check to see
## if the solution has already been generated.  If it has not been, then the solution will be found and stored in the
## cache.  If the solution has been generated, then it will be retrieved from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invert <- x$getinv()
  if(!is.null(invert)) {
    message("getting cached data")
    return(invert)
  }
  data <- x$get()
  invert <- solve(data, ...)
  x$setinv(invert)
  invert
}
