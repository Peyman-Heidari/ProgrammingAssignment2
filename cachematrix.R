## Put comments here that give an overall description of what your
## functions do

## The makeChacheMatrix function will make the list that holds 
## the set, get, setsolve, and getsolve functions.

makeCacheMatrix <- function(x = matrix()) {
  so <- NULL
  set <- function(y) {
    x <<- y
    so <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) so <<- solve
  getsolve <- function() so
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)

}



## The cacheSolve function return inverse of a square invertible matrix. 
## However, it will use the cached value if the inverse has already been calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  so <- x$getsolve()
  if(!is.null(so)) {
    message("getting cached data")
    return(so)
  }
  data <- x$get()
  so <- solve(data, ...)
  x$setsolve(so)
  return(so)
}
