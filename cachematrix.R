## The functions 'makeCacheMatrix' and 'cacheSolve' cache the inverse of a matrix.

## This function creates a special "matrix" object, that is actually a list, that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(I) inv <<- I
        getInv <- function() inv
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse
# has already been calculated (and the matrix has not changed),
# then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- I(data, ...)
        x$setInv(inv)
        inv
}
