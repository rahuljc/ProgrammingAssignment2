## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix- initializes the cache variable "inverse" and 
##                   a bunch of helpful functions {set,get,setinverse,getinverse}
## Input parameters: A matrix
## Returns: a vector of functions.
##

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL # The inverse of the matrix gets cached in this variable.
        
        set <- function(y) { #useful to set a matrix ; for testing purposes
                x <<- y
                inverse <<- NULL
        }
        get <- function() {
                x
        }
        setinverse <- function(inv) { # sets the cached variable with value from cacheSolve
                inverse <<- inv
        }
        getinverse <- function() { # just returns the cached variable
                return (inverse)
        }
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## The purpose of this function is to return the inverse of a matrix intelligently.
## First, this call getinverse to get the value of the cached variable.
## Second, this checks if the variable is not empty. 
##          If NOT empty, return the variable as it is.
##          Else, get the input matrix, find its inverse and set it in cache variable.
## Finally, return the cached variable.
## 

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