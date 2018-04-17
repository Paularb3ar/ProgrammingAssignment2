## Caching the Inverse of a Matrix: The functions aim to cache the inverse of a matrix to avoid costly computations

## My first function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  #get the inverse by making use of solve in base r, assuming an invertible matrix is supplied
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## My second function computes the inverse of the returned matrix by the makeCacheMatrix function above
## If the inverse has already been calculated it will instead be retrieved from the cache

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  #get the inverse by making use of solve in base r, assuming an invertible matrix is supplied
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
