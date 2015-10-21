##These functions allows to cache the result of inversing a matrix and retrieve it if needed

## makeCacheMatrix takes a matrix and creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix
    ##If the inverse has already been calculated (and the "matrix" is the same), 
    ##then the cachesolve retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data)
    x$setsolve(s)
    s
}
