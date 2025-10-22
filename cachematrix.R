##makeCacheMatrix: creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # Set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Clear cached inverse when matrix changes
  }
  
  # Get the matrix
  get <- function() x
  
  # Set the inverse
  setInverse <- function(inverse) inv <<- inverse
  
  # Get the inverse
  getInverse <- function() inv
  
  # Return a list of methods
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function returns the inverse of the matrix created with makeCacheMatrix. 
If the inverse is already cached and the matrix hasn’t changed, it retrieves it from the cache

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  # If inverse is already cached
  if (!is.null(inv)) {
    message("Getting cached inverse...")
    return(inv)
  }
  
  # Otherwise, calculate the inverse
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
