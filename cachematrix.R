## This set of functions return the value of the inverse of a specified
## matrix. This value is stored in the special "matrix", which is the
## output of the first function (makeCacheMatrix). The second function 
## (cacheSolve) returnsthe inverse of the matrix "x", specified by the user
## (the user also has to specify the number of rows and columns in the matrix).
## If this inverse has already been calculated, then cacheSolve retrives this
## value from the output of the makeCacheMatrix.

## makeCacheMatrix outputs a list which contains information to set and retrieve
## values corresponding to the input matrix as well as its inverse.

makeCacheMatrix <- function(x = matrix(),...) {
    i <- NULL
    set <- function(y = matrix(),...) {
    x <<- y
    i <<- NULL
  }
  get <- function() matrix(x,...)
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



## cacheSolve calculates and outputs the inverse of the matrix "x". If this value
## has already been calculated, it retrieves this value from the output of
## makeCacheMatrix and outputs it.

cacheSolve <- function(x, ...) {
    a <- x$getinv()
    if(!is.null(a)) {
      message("getting cached data")
      return(a)
    }
    data <- x$get()
    a <- solve(data, ...)
    x$setinv(a)
    a
  }
