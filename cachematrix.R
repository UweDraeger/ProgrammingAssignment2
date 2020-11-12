## A pair of functions that cache the inverse of a matrix.

## makeCacheMatrix function creates a special "matrix" object that can
## cache its inverse.
makeCacheMatrix <- function(x) {

        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        setsolve <- function(solve) inv <<- solve
        getsolve <- function() inv
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve 
## retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {

        inv <- x$getsolve()
        if(!is.null(inv)) {
                message("getting cashed data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setsolve()

        ## Return matrix that is the inverse of 'x'
        inv
        
}



