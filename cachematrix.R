## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly.
## The following pair of functions caches inverse matrix

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setdest <- function(dest) i <<- dest
        getdest <- function() i
        list(set = set,
             get = get,
             setdest = setdest,
             getdest = getdest)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	i <- x$getdest()
    if (!is.null(i)) {
        message("cached data")
        return(i)
        }
    data <- x$get()
    i <- solve(data, ...)
    x$setdest(i)
    i
}