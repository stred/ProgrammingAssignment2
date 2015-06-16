## These function will store a solved version of a matrix in an alternate environment and 
## allow that cached version to be recalled.
##
## The functions can be nested:
##
## solvedValue <- cacheSolve(makeCacheMatrix(testMatrix))

## makeCacheMatrix
## this function creates a list of functions to get and set a matrix and get and set the solve'd version

makeCacheMatrix <- function(x = matrix()) {
   # Clear our function variables
   rm <- NULL

   set <- function(y) {
      # sets x to the value of the passed y
      x <<- y
      # Clear our function variables
      rm <<- NULL
   }
   get <- function() {
      # return's x
      x
   }
   setsolve <- function(solve) { 
      # calculates the solve of x and assigns to rm
      rm <<- solve
   }
   getsolve <- function() {
      # returns the cached rm value
      rm
   }
   # return the list of functions
   list(set = set, get = get,
        setsolve = setsolve,
        getsolve = getsolve)
}


## cacheSolve
## this function takes a cacheMatrix and either returns the cached solve'd version or calculates it

cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
   rm <- x$getsolve()
   if(!is.null(rm)) {
      # return the cached data and exit
      return(rm)
   }
   # retrieve the matrix from the cacheMatrix and solve it
   data <- x$get()
   rm <- solve(data, ...)
   
   # set our new solve'd value as the cached value
   x$setsolve(rm)
   
   # return the solve'd matrix
   rm
}
