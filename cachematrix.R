## These two function will calculate the inverse of an invertible matrix.
## If the inverse has already been calculated then the inverse will be retrieved

## This function creates a matrix object and will place the inverse 
##of the numeric matrix in the cache. 
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinvmatrix <- function(invmatrix) m <<- invmatrix
  getinvmatrix <- function() m
  list(set = set, get = get,
       setinvmatrix = setinvmatrix,
       getinvmatrix = getinvmatrix)
}

## This function returns the inverse of the special "matrix" from 
##function make ChacheMatrix.  If the inverse has already been calculated
##then the inverse is retrieved from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinvmatrix()
  print(m)
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinvmatrix(m)
  m
}
