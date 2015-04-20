## Functions for caching and accessing the inverse of a matrix.

## Creates a special matrix allowing the value of its inverse to be cached.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
      x <<- y
      inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Takes a special matrix and returns its inverse.
## If the inverse has already been cached then the cached value is returned.
## If not then the inverse is calculated, returned and cached.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  
  if(!is.null(inv)) {
      message("Getting cached data")
      return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
