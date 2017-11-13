## We use these two below functions to check if the inversion of a matrix 
## has been deposited in cache. If it is, it can be looked up in 
## the cache rather than recomputed


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inver <<- solve
  getinverse <- function() inver
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
