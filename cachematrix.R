## This file contains two functions for working with matrices: one which turns
## a matrix into a "special matrix object" and one for reading the inverse of the matrix.

## This function will create an object consisting of four functions which
## get and set the matrix passed in the argument and the matrix inverse.
makeCacheMatrix <- function(m = matrix()) {
        inverse <- NULL
        ## Assigns a value to the m (matrix) variable, which is in the parent environment.
        set <- function(y) {
                m <<- y
                inverse <<- NULL
        }
        ## Gets the m (matrix) variable.
        get <- function() {
                m
        }
        ## Assigns a value to the inverse variable, which is in the parent environment.
        setsolve <- function(solve) {
                inverse <<- solve
        }
        ## Gets the inverse variable.
        getsolve <- function() {
                inverse
        }
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## This function computes the inverse of the matrix that's in the "special matrix object"
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the function retrieves the inverse from the cache.
cacheSolve <- function(cachematrix) {
        inverse <- cachematrix$getsolve()
        if (!is.null(inverse)) {
                message("getting inverse from cache")
                return(inverse)
        }
        data <- cachematrix$get()
        inverse <- solve(data)
        cachematrix$setsolve(inverse)
        inverse
}
