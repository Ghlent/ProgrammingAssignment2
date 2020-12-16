## The next functions allow the user to solve the inverse of a matrix and
## store it in the cache for later use.

## The first function makes a list of functions for the creation of a matrix (x),
## the storage of the inverse (s) and the recovery of those values.

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve<- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## The second function checks if the inverse of a matrix has already been
## calculated; if yes, it gets it from cache, if not, it solves it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}
