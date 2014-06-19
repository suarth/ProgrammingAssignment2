## Description: 2 functions that help cache an inverse of a matrix.
## Written by Arthur Su for coursera.org course "R Programming" assignment 2

## This function makes a list (or vector) of functions that will ultimately
## help cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- solve(x)
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function executes the created functions in the list (or vector)
## to find the inverse if its not already cached in the past.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}