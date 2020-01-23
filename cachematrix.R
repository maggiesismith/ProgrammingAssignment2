## These functions are used to cache the inverse of a matrix so that it does
## not have to be calculated repeatedly.

## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y #assign input argument to the x object in parent environment
    m <<- NULL #assign NULL to m object in parent environment (clears previous values of m)
  }
  get <- function() x #retrieve x from the parent environment
  setInv <- function(inverse) m <<- inverse
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## This function checks to see if the inverse of the matrix has been calculated
## and if so, retrieves it from the cache. If not, it computes the inverse
## of the matrix from the above function.

cacheSolve <- function(x, ...) {
  m <- x$getInv() #attempting to retrieve inverse from the object
  if(!is.null(m)) { #if the inverse is cached, return the inverse
    message("getting cached data")
    return(m)
  }
  #otherwise, calculate the inverse
  data <- x$get()
  m <- solve(data, ...) #calculate inverse
  x$setInv(m)
  m
}
