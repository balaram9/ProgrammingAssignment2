# The makeCachematrix function returns the inverse of the matrix. It initially checks for computed inverse. 
# If computation is already done, it gets the result and skips further
# computation. If not, it computes the inverse and sets the value in the cache via
# setinverse function.

makeCacheMatrix <- function(x = matrix()) {inv <- NULL
set <- function(y) {
  x <<- y
  inv <<- NULL
}
get <- function() x
setinverse <- function(inverse) inv <<- inverse
getinverse <- function() inv
list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}

## The following function assumes that the matrix is always invertible

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
        ## Returns a matrix that is the inverse of 'x'
}
