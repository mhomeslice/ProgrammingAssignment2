
## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object stored in a new environment
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function (y){
                x <<- y
                m <<- NULL
        }
        ##set the value of the matrix
        get <- function() x  ## get the value of the matrix
        setinverse <- function (inverse) m<<- inverse ## set the value of the inverse
        getinverse <- function() m ##get the value of the inverse
        list(set = set, 
             get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}

## Calculates the inverse of a matrix, checks to see if it has been calculated
## if so - get mean from cache
## in not - calculates the inverse, and then stores in cache

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if (!is.null (m)){
                message ("getting from cached data")
                return (m)
        }
        data <- x$get()
        m <- solve (data, ...)
        x$setinverse(m)
        m
        ## Return a matrix that is the inverse of x
}
