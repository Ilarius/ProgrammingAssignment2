## Matrix inversion can be heavy computationally. This function allows you to save in cache a matrix inversion if already computed.

## makeCacheMatrix create a special "matrix" objects that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been computed, then cache is retrieved

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("no computation: retrieving cache")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m  ## Return a matrix that is the inverse of 'x'
}
