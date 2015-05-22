## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
          x <<- y
          inverse <<- NULL
        }
        get <- function() {
          x
        }
        setinverse <- function(inv) {
          inverse <<- inv
        }
        getinverse <- function() {
          return (inverse)
        }
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("Thank God, Its Cached ! No work here")
  }else{
    message("Nope! nothing in cache . Will have to recompute.")
    data <- x$get()
    inverse <- solve(data)
    x$setinverse(inverse)
  }
  return (inverse)
}
