## These two function s work together to cache data that may be expensive to run repeatedly.

## This function returns a list object of functions to be used by the cacheSolve function

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


## This function returns the inverse of a matrix that it is given.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'


    m <- x$getsolve()
    if(!is.null(m)) {
        message("you already ran this, so i am using the cache")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
