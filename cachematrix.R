## This code is modified from the makeVector and cachemean functions
## written by Peng et al. for the Programming in R Coursera class


## This function makes a matrix that can also cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # Returns original matrix
  get <- function() x
  
  # Sets inverse, from cacheSolve below
  setInverse <- function(inverse) m <<- inverse
  
  # Returns inverse after it is set
  getInverse <- function() m
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Computes, caches, and returns matrix inverse. 
## If already cached, shows message indicating as such
cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached inverse matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  m
}