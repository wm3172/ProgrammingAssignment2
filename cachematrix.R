## Matrix inversion functions to cache the inverse of a matrix instead of
## computing it repeatedly
## Cache the inverse of a matrix so that when we need it again it can be
## looked up in the cache rather than recomputed

## makeCacheMatrix() function creates a special "matrix" object that can cache
## its inverse
## Contains the following functions:
## (a) set() - set the value of the matrix
## (b) get() - get the value of the matrix
## (c) setInverse() - set the value of the matrix inverse 
## (d) getInverse() - get the value of the matrix inverse
makeCacheMatrix <- function(aMatrix = matrix()) {
    cachedInverse <- NULL
    set <- function(aNewMatrix) {
        aMatrix <<- aNewMatrix
        cachedInverse <<- NULL
    }
    get <- function() aMatrix
    setInverse <- function(aInverse) cachedInverse <<- aInverse
    getInverse <- function() cachedInverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## cacheSolve() function computes the inverse of the special "matrix" returned
## by the makeCacheMatrix() function.
## If the inverse has already been calculated then inverse can be retrived
## from the cache
cacheSolve <- function(aMatrix, ...) {
    mInverse <- aMatrix$getInverse()
    if(!is.null(mInverse)) {
        message("getting cached data")
        return(mInverse)
    }
    data <- aMatrix$get()
    mInverse <- solve(data, ...)  ## computes inverse
    aMatrix$setInverse(mInverse)
    mInverse
}
