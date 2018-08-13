

## This function caches the inverse matrix of the matrix inputed. 

makeCacheMatrix <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinvertedmatrix <- function(solve) m <<- solve
        getinvertedmatrix <- function() m
        list(set = set, get = get,
             ssetinvertedmatrix = setinvertedmatrix,
             getinvertedmatrix = getinvertedmatrix)
}

## cacheSolve recieves the cached inverted matrix from makeCache Matrix. If there is no cached matrix, cacheSolve solves for the inverted matrix on its own.

cacheSolve <- function(x, ...) {
        m <- x$getinvertedmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$ssetinvertedmatrix(m)
        m}