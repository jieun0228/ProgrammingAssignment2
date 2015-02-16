# Assignment: Caching the Inverse of a Matrix
# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#             If the inverse has already been calculated (and the matrix has not changed), 
#             then the cachesolve should retrieve the inverse from the cache
# <<- operator which can be used to assign a value to an object in an environment 
#     that is different from the current environment.


makeCacheMatrix <- function(x= matrix()) {
  
  #initialize the inverse of x
  inv <- NULL

  set <- function(y) {
    x <<- y
    inv <<- NULL
  }

  get <- function() x
  setInverse <- function(solve) inv <<- solve
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}

cacheSolve <- function (x, ...) {
  
  #x is a special matrix, makeCacheMatrix()
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)    
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv  
}
