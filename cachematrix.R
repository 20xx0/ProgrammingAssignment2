## These functions are handy in order to reduce computing power
## when doing certain complex calculations, such as in this case
## finding the inverse of a matrix. The functions cache the "solved"
## matrix so that when we need it, it can be looked up from
## the cache.

## This function makes a special "cache matrix", containing the
## inverse of the input.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function "solves" the above matrix. If it has already
## been solved, then it takes the value from the cache, instead
## of computing it. If it has not been solved, then it solves it
## like usual.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m   ## Return a matrix that is the inverse of 'x'
}