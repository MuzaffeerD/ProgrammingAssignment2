## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # todo error if x is not a matrix
  inv <- NULL
  set <- function(y) {
    g <<- y
    inv <<- NULL
  }
  get <- function() g
  setinv <- function(i) inv <<- i
  getinv <- function() inv
  list(set = set, get = get,setinv = setinv, getinv = getinv)
}

## Write a short comment describing this function

#' Compute and cache the inverse of a matrix
#' Parameter - g is the result of a previous makeCacheMatrix call
#' Parameter -  ... additional arguments to pass to solve function
#' examples
#' g = makeCacheMatrix(matrix(rnorm(9), 3, 3))
#' cacheSolve(g)


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'g'
    inv <- g$getinv()
    if(!is.null(inv)) {
      message("getting cached matrix inverse")
      return(inv)
    }
    data <- g$get()
    inv <- solve(data, ...)
    g$setinv(inv)
    inv
}
