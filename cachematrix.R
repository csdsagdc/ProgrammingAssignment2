## These two functions create a list to store a matrix and cache its inverse

## This function creates a list with a function to: 
##      1. Set the matrix
##      2. Get the matrix
##      3. Set the inverse of the matrix
##      4. Get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        setmatrix <- function(y) {
                x <<- y
                m <<- NULL
        }
        getmatrix <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(setmatrix = setmatrix, getmatrix = getmatrix,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The following calculates the inverse of the matrix set in the above function.
## If the inverse is already stored in cache, it retrieves it. If not, it calculates
## the inverse then sets it in the cache via the setinverse function.

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached inverse")
                return(m)
        }
        data <- x$getmatrix()
        m <- solve(data)
        x$setinverse(m)
        m
}