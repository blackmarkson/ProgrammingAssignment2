## Matrix inversion is a costly computation, 
## These two functions cache the inverse rather than repeatedly compute it.

## The function makeCacheMatrix creates a special "matrix" object 
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  ##
  ## Takes an input a square matrix that is assumed to be invertible
  ##
  ## Returns a List object that holds getter and setter functions to
  ## get and set the matrix, and to
  ## get and set the inverse of the matrix
  ##
  ## The cachesolve function uses this List to cache the matrix inverse
  
  inverse <- NULL
  set <- function(y) {
    x <<- y  # the matrix is assigned to a separate environment
    inverse <<- NULL
  }
  
  get <- function() x    # return the matrix from this environment
  
  # the inverse is also assigned to this separate environment
  
  setinverse <- function(inv) inverse <<- inv
  
  getinverse <- function() inverse  # return the inverse
  
  # return the list object
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The function cacheSolve computes the inverse of the special "matrix" 
##   returned by makeCacheMatrix above. 
## If the inverse has already been calculated,
##   then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inverse <- x$getinverse()
  
  # if the inverse has been cached
  
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse) # return the cached inverse
  }
  
  # no cache value, so calculate the inverse, and add to  cache
  
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse) # add to cache
  
  # return inverse
  
  inverse

}
