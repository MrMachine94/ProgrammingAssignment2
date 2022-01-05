## This is a pair of functions that is used to create special object
## that stores special matrix object and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ## To initiate inverse as Null
        inv <- NULL 
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        ## Function to get matrix x
        get <- function() x
        setInverse <- function(inverse) {inv <<- inverse}
        getInverse <- function() {inv} ## To obtain inverse of matrix
        list(set = set,get = get,setInverse = setInverse,getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        ## Checking if the inverse is NULL
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
