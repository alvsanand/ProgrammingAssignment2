# This function creates a special "matrix" object that can cache its solve.
#
# Args:
#   x: the "matrix" object to be used by the matrix wrapper.
#
# Returns:
#   The wrapper matrix.
makeCacheMatrix <- function(x = matrix()) {
  #the cached solve of the matrix
  sol <- NULL
  
  # This function sets a new "matrix" object.
  #
  # Args:
  #   x: the new "matrix" object to be used by the matrix wrapper.
  set <- function(y) {
    x <<- y
    sol <<- NULL
  }
  
  # This function returns the "matrix" object.
  #
  # Returns:
  #   The "matrix" object.
  get <- function() x
  
  
  # This function sets a new "matrix" object.
  #
  # Args:
  #   x: the new "matrix" object to be used by the matrix wrapper.
  #
  # Returns:
  #   The wrapper matrix.
  setsolve <- function(s) sol <<- s
  
  # This function returns the solve of the "matrix" object.
  #
  # Returns:
  #   The solve of the "matrix" object.
  getsolve <- function() sol
  
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


# This function return the solve of the matrix wrapper.
#
# Args:
#   x: the matrix wrapper.
#
# Returns:
#   The solve of wrapper matrix.
cacheSolve <- function(x, ...) {
  sol <- x$getsolve()
  if(!is.null(sol)) {
    message("getting cached data")
    return(sol)
  }
  data <- x$get()
  sol <- solve(data, ...)
  x$setsolve(sol)
  sol
}
