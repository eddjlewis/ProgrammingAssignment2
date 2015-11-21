## These two functions makeCacheMatrix and cacheSolve are used together to store a matrix together with its
# inverse. When an inverse is required, it is returned from cache where possible.
# Example use:
#
# myMatrix <- makeCacheMatrix(matrix(c(4,3,3,2), nrow = 2, ncol = 2))
# myMatrix$get() # returns my matrix
# cacheSolve(myMatrix) # calculates inverse of matrix and returns and caches it
# cacheSolve(myMatrix) # returns inverse from cache the second time

## makeCacheMatrix is a function which takes a matrix 'x' as an argument and returns a list of four functions:
# get - returns original matrix 'x'
# set - allows matrix 'x' to be updated
# getInverse - returns inverse of 'x' - if it has been set by the cacheSolve function
# setInverse - allows inverse of 'x' to be set - used by the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(mean) m <<- mean
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
}

## Takes a list of the type created by the makeCacheMatrix function. Returns the inverse of that matrix
# If the result is cached, it returns from cache
# If the result is not cached, it is calculated and then cached and returned

cacheSolve <- function(x, ...) {
        
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
