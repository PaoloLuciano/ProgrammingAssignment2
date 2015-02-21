## Since inverting a Matrix requires a lot of computation, it is easier to "cache" data in order to save a lot of computation power.
## This couple of functions allow you to create a "matrix" get its inverse and to save it for future calculations.
## If the program doesn't find the cached matrix, it will get its inverse

## The first function creates a "matrix" that is actually a list of four functions (sort of an object), 
## that allow us to perform diferent procedures to the given matrix and it's inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse  <-  NULL
    set  <-  function(y){
        x <<- y
        inverse <<- NULL
    }
    get <- function(){
        x
    }
    setinverse <- function(solve){
        inverse <<- solve
    }
    getinverse <- function(){
        inverse
    }
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function, checks if there is already a cached inverse of the matrix "x", if not it is calculated and stored.
## In both cases it returns the inverse of it

cacheSolve <- function(x , ...){
    inverse <- x$getinverse()
    if (!is.null(inverse)){
        message("Getting cached inverse matrix")
        return (inverse)
    }
    data <- x$get()
    inverse  <- solve(data , ...)
    x$setinverse(inverse)
    inverse
}

