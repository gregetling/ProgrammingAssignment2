## These functions implement a caching function for the solve() command by operating
## on a new list object containing constructor methods

## Prototypes:
## makeCacheMatrix(x = matrix())
## cacheSolve(x, ...)

## Usage:
## a <- makeCacheMatrix(matrix(c(...), ncol = #, nrow = #))
## cacheSolve(a)

## makeCacheMatrix() - Constructor object with get/set methods
## used to store original data and new values

makeCacheMatrix <- function(x = matrix()) {
  
  # Initialization, set(), and get() for object are identical to example
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  
  # setinverse() is functionally identical to example
  setinverse <- function(inverse) inv <<- inverse
  
  # getinverse() is functionally identical to example
  getinverse <- function() inv
  
  # Return the object to the caller
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## cacheSolve() - Solver function that takes a makeCacheMatrix object
## only runs the solve on cache miss

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  # Get inverse from cache
  inv <- x$getinverse()
  
  # If inverse exists in cache, return it
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Inverse not set in cache, get the data from the object
  data <- x$get()
  
  # Compute the inverse
  inv <- solve(data, ...)
  
  # Cache the inverse and return it
  x$setinverse(inv)
  inv
}
