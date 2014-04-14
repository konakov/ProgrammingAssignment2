## Sergei Konakov, course "R Programming", 2014
## https://github.com/konakov/ProgrammingAssignment2

## Matrix inversion is usually a costly computation and there may be
## some benefit to caching the inverse of a matrix rather than
## compute it repeatedly.

## This file contains two functions, the first (makeCacheMatrix) allows
## creation of a spetial matrix object, with attached functions and
## cached inverse matrix.
## The second (cacheSolve) produce the inverse, if no iverse yet,
## or just returns the cached inverse of a matrix.

## Usage examples
##
## x1 <- makeCacheMatrix(matrix(rnorm(4000000), nrow=2000, ncol=2000))
## the matrix must be square (ncol == nrow) to be inversable
## the matrix must not be trivial, so matrix(1:4000000,... is no good,
## but matrix(rnorm(4000000),... is
##
## cacheSolve should provide comparable results (these are from
## AMD Athlon(tm) 64 X2 Dual Core Processor 4600+ × 2 ; 4GB RAM)
##
## x1 is from the above, i.e. it's a 2000 × 2000 matrix
##
## > system.time(slv <- cacheSolve(x1))
##    user  system elapsed
##  38.267   0.625  39.025
##
## > system.time(slv <- cacheSolve(x1))
## Found in the cache!
##    user  system elapsed
##   0.000   0.000   0.001
##


makeCacheMatrix <- function(x = matrix()) {

  s <- NULL ## define the solution and make it empty

  set <- function(y) { ## create a new matrix
    x <<- y
    s <<- NULL ## invalidate cache!
  }

  get <- function() x ## return the matrix

  setsol <- function(sol) s <<- sol ## store the solution

  getsol <- function() s ## return the SOLUTION

  list(set = set, get = get, ## show what was created
    setsol = setsol,
    getsol = getsol)

}


## If the matrix is already solved, and the matrix has not changed since,
## return inverse form cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getsol()
  if(!is.null(s)) {
    message("Found in the cache!")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsol(s)
  s

}
