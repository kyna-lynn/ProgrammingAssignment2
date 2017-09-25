## The following two functions together cache the inverse of a matrix.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## The output of the function is a list containing a function to:
#a. set the value of the matrix
#b. get the value of the matrix
#c. set the value of the inverse of the matrix
#d. get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y = matrix()){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated, then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv   ## Return a matrix that is the inverse of 'x'
}
