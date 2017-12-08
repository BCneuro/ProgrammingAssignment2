## Together, these two functions provide the ability to invert a matrix,
## cache the inverted matrix, check whether there is already a value
## cached, and calculate a new one if there is not.

## The first function creates two objects.  The first is x, a function
## that remains to be defined.  The second is invmat, where the 
## inverse matrix can be stored.  The get and set functions define where
## the values/functions that populate the objects will come from, and 
## allows previous values to be overwritten.

makeCacheMatrix <- function(x = matrix()) {
  invmat <- NULL
  set <- function(y) {
    x <<- y
    invmat <<- NULL
  }
  get <- function() x
  setinv <- function(invert) invmat <<- invert
  getinv <- function() invmat
  list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## This function checks to see whether there is already a cached matrix 
## available for invmat.  If there is, it prints "retrieving cached data"
## and retrieves the cahced matrix.  If not, it uses the solve function
## to invert the matrix, and prints and caches this new matrix.

cacheSolve <- function(x, ...) {
  invmat <- x$getinv()
  if(!is.null(invmat)) {
    message("retrieving cached data")
    return(invmat)
  }
  data <- x$get()
  invmat <- solve(data)
  x$setinv(invmat)
  invmat
}



