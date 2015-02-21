##
## This function creates a special "matrix" object that can cache its inverse.
## The function returns a list containing the following functions that operate on the cache
## setMatrix - sets the value of the matrix
## getMatrix - gets the value of the matrix
## setInverse - sets the value of the inverse matrix
## getInverse - gets the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    setMatrix <- function(y) {
        x <<- y
        m <<- NULL
    }
    getMatrix <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)
}

## The following function calculates the inverse of the special "matrix" object 
## created with the above function "makeCacheMatrix". However, it first checks 
## to see if the inverse has already been calculated. If so, it gets the inverse 
## from the cache and skips the computation. Otherwise, it calculates the inverse 
## of the data and sets the value of the inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$getMatrix()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}

##
## Example Usage  (testcase)
##
## m <- matrix(c(1,1,1,3,4,3,3,3,4), 3,3)
## myObject <- makeCacheMatrix(m)
## cacheSolve(myObject)
## m %*% cacheSolve(myObject)    Returns the identity matrix
##

