## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a matrix object that can cache its
## inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinversematrix <- function(matrix) m <<- matrix
    getinversematrix <- function() m
    list(set = set, get = get, setinversematrix = setinversematrix, getinversematrix = getinversematrix)
}


## cacheSolve computes the inverse of the matrix created by
## makeCacheMatrix. If the inverse has already been calculated,
## the function retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinversematrix()
    if(!is.null(m)) {
        message("Getting inverse from cache")
        return(m)
    }
    m <- solve(x$get())
    x$setinversematrix(m)
    m
}
