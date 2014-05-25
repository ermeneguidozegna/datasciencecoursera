## These two functions create a matrix that can chache its inverse.
## The second function checks whether an inverse of a matrix have been computed.
## If it has, then it gets the inverse from the cache. Otherwise
## it computes the inverse of the matrix

## makeCacheMatrix is used to create a matrix object
## that can cache the result of its inverse

makeCacheMatrix <- function(x = matrix()) {
  slv <- NULL
  set <- function(y) {
    x <<- y
    slv <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) slv <<- solve
  getsolve <- function() slv
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)

}


## cacheSolve is used to check whether the inverse of a matrix has been computed
## If it has, it retrieve the inverse from the cache
## Otherwise, it computes the inverse

cacheSolve <- function(x, ...) {
        
  slv <- x$getsolve()
  if(!is.null(slv)) {
    message("getting cached data")
    return(slv)
  }
  data <- x$get()
  slv <- solve(data, ...)
  x$setsolve(slv)
  slv
}
