## The first function makes a list with methods that set and get a matrix and its inverse.
## The second function passes the list from the first and attempts to calculate and set its inverse.  
##If the inverse is already set, the cached value is used.

## makeCacheMatrix will create matrix X and show three methods to set/get X and its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
   }


## Given the list variable from the first function, will first check to see if there is already a cached inverse and return
## otherwise will attempt to solve its inverse and set/return it using cacheSolve

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
## end of program.