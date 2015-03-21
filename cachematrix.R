## The following code is to write a pair of functions that can cache the inverse of a matrix.


## This function(makeCacheMatrix) creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x=matrix()) {
  ## Creates a list of functions that can cache the inverse of a matrix.
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<-inverse
  getInverse <- function() m
  list(set = set, get = get,setInverse = setInverse,getInverse = getInverse)
  
}

## This function(cacheSolve) will inverse of the matrix returned by makeCacheMatrix(), 
## unless the inverse has already been calculated, in which case it retrieves it from the cache. 
cacheSolve <- function(x, ...) {
  ## x is output of makeCacheMatrix()
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if ( ! is.null(m)) {
    print("getting cached data")
    return(m)
  }
  m <- solve(x$get())
  x$setInverse(m)
  m
}