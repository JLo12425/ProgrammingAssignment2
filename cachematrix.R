## This pair of functions that cache the inverse of a matrix

## The makeCacheMatrix function creates an object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL
      set <- function(y){
            x <<- y
            inverse <- NULL
      }
            
      get <- function() x
      setinverse <- function(solve) inverse <<- solve
      getinverse <- function() inverse
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The cacheSolve function first checks to see if the inverse is stored in the cache,
## retrieves the inverse if it is stored, and if not, computes and returns the inverse

cacheSolve <- function(x, ...) {
      inverse <- x$getinverse()
      if(!is.null(inverse)){
            message("getting cached inverse")
            return(inverse)
      }
      data <- x$get()
      inverse <- solve(data)
      x$setinverse(inverse)
      inverse
}
