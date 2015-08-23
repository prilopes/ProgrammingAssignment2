## Using functions to cache the inverse of a matrix

## makeCacheMatrix creates a speacial "matrix" object 
## that can cacha its inverse
## contains functions to:
## - set the values of a matrix
## - get the values of a matrix
## - set the inverse of the matrix
## - get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve computes the inverse of a matrix created
## with makeCacheMatrix. If the inverse is already cached
## the function retrieves the inverse, otherwise it uses
## the solve function to compute the inverse and caches it

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
