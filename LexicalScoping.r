#The "makeCacheMatrix" function creates a special array object,
#and then the "cacheSolve" function calculates the inverse of the array.
#If the inverse of the array has already been computed,
#it is found in the cache and returns it, with no need to recalculate it.

makeCacheMatrix <- function(x = matrix())
{
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}	 
# cacheSolve is a function which computes the inverse of the special "matrix" 
#returned by makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve the 
# inverse from the cache

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached result")
    return(inv)
  }
  +  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}	 