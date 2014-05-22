## CacheMatrix
## created: 2014.05.22T22:52:00GMT
## createdby: jb1t

## The functions below will allow you to create a matrix and calculate its inverse
## The calculated value for that matrix will be cached, so the subsequent 
## calls to cacheSolve with the same matrix will not calculate the inverse again, however, 
## return the saved value.

## NOTE: These functions assume that the matrix supplied is always invertible.

## Creates a "special" matrix that can be used with cacheSolve
## The matrix in use with cacheSolve will cache the resulting inverse matrix.
makeCacheMatrix <- function(x = matrix()) {
    im <- NULL
    set <- function(y) {
        x <<- y
        im <<- NULL
    }
    get <- function() x
    setinverse <- function(inverseMatrix) im <<- inverseMatrix
    getinverse <- function() im
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    im <- x$getinverse()
    if(!is.null(im)) {
        message("getting cached data")
        return(im)
    }
    data <- x$get()
    im <- solve(data, ...)
    x$setinverse(im)
    im
}