## The makeCacheMatrix function returns an object (a list) that has the
## ability to return the inverse of the given matrix 'x'.  
## Call the cacheSolve function passing the object returned by makeCacheMatrix
## in order to access the inverse of the matrix 'x'.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(mean) m <<- mean
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Calling the cacheSolve(x) function passing the object returned from 
## makeCacheMatrix will return the inverse of the given matrix 'x'.  
## Upon the first call to cacheSolve, the inverted matrix will be cached.  
## On subsequent calls, the previously inverted matrix is returned from 
## the cache thus improving performance.  
##
## If the supplied matrix is replaced with a call to the set(y) function,
## the inverse of the new matrix will be calculated and cached on the 
## next call to cacheSolve.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
