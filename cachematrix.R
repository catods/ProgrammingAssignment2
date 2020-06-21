## This sets of functions allow to create a data structure to manipulate
## a matrix, caching the inverse of the matrix, so it does not need to
## be re-calculated when you call the function with the same argument

## makeCacheMatrix creates a list structure of functions that allow get
## and set the matrix and the inverse of the matrix. When getting the
## inverse of the matrix, it returns the value in cache if it was previously
## calculated.

makeCacheMatrix <- function(x = matrix()) {
  solve_x <- NULL
  set <- function(y) {
      x <<- y
      solve_x <<- NULL
  }
  get <- function() x
  setsolve <- function(new_solve_x) solve_x <<- new_solve_x
  getsolve <- function() solve_x
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve calculates the inverse of the matrix passed as part of the
## object returned by the makeCacheMatrix function, returning the inverse matrix
## in cache (if any) or calculating the inverse and storing that in the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    solve_x <- x$getsolve()
    if(!is.null(solve_x)) {
        message("getting cached data")
        return(solve_x)
    }
    data <- x$get()
    solve_x <- solve(data, ...)
    x$setsolve(solve_x)
    solve_x
}
