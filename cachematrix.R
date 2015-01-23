## This scripts returns the inverse of a square matrix, cacheing the step value for efficient computation

## Create a special matrix that stores the step inverse value of the matrix being solved

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Calculates the inverse of matrix x, checking first if a cached value already exist

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
