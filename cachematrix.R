# PROGRAMMING ASSIGNMENT 2 - R Programming Course
# by Princess Nicole Oriola (hyola)

## FUNCTION 1 creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(m = matrix()) {
  
  inv_cache <- NULL # to serve as a flag indicating that the cache is empty
  
  # set the matrix
  set <- function(new_matrix) {
    m <<- new_matrix
    inv_cache <<- NULL
  }
  
  # get the matrix
  get <- function() {
    m
  }
  
  # set the cached inverse
  setInverse <- function(inverse) {
    inv_cache <<- inverse
  }
  
  # get the cached inverse.
  getInverse <- function() {
    inv_cache
  }
  
  # return a named list of the four methods (functions)
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## FUNCTION 2 computes the inverse of the special "matrix" object returned by makeCacheMatrix().
cacheSolve <- function(x, ...) {
  
  inv <- x$getInverse() # attempts to retrieve the inverse from the object's cache.
  
  ## CASE 1
  # If the inverse has already been calculated (and the matrix has not changed),
  # retrieve the inverse from the cache and skip the the rest of the function.
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## CASE 2
  # If the cache was empty (inv was NULL), we must compute the inverse..
  data <- x$get()
  
  # solve() function is R's standard tool for linear algebra 
  inv <- solve(data, ...)
  
  # Now, cache the newly computed inverse.
  x$setInverse(inv)
  inv
}