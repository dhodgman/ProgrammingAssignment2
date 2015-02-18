## The makeCacheMatrix function returns an object (a list) that has the
## ability to return the inverse of the given matrix.  
##
## Calling the cacheSolve(x) function passing the object returned from 
## makeCacheMatrix will return the inverse of the given matrix.  Upon the 
## first call to cacheSolve, the inverted matrix will be cached.  On 
## subsequent calls, the previously inverted matrix is returned from the 
## cache thus improving performance.  
##
## If the supplied matrix is replaced with a call to the set(y) function,
## the inverse of the new matrix will be calculated and cached on the 
## next call to cacheSolve.

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


## Return a matrix that is the inverse of 'x'
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
