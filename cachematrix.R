## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  #
  # This function creates a special "matrix" object that can cache 
  # its inverse.
  #
  t <- NULL
  set <- function(y) {
    x <<- y
    t <<- NULL
  }
  get <- function() x
  setsolv <- function(solv) t <<- solv
  getsolv <- function() t
  list(set = set, get = get,
       setsolv = setsolv,
       getsolv = getsolv) 
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  #
  # This function computes the inverse of the special "matrix" returned 
  # by makeCacheMatrix above. If the inverse has already been calculated 
  # (and the matrix has not changed), then the cachesolve should retrieve
  # the inverse from the cache.
  #
  t <- x$getsolv()
  if(!is.null(t)) {
    message("getting cached data for the inverse matrix")
    return(t)
  }
  data <- x$get()
  t <- solve(data, ...)
  x$setsolv(t)
  t
}
