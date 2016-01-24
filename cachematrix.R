## This function returns a "cache matrix" which is basically a matrix with 
## functions to get/set the matrix/inverse. The calculation for the inverse
## is done in a separate function (cacheSolve) below.

makeCacheMatrix <- function(x = matrix()) {
        i <- NA
        set <- function(y) {
                x <<- y
                i <<- NA
        }
        get <- function() { x }
        setinverse <- function(inverse) { i <<- inverse }
        getinverse <- function() { i }
        list(set = set, get = get, 
             setinverse = setinverse, getinverse = getinverse)
}

## This function is meant to work with makeCacheMatirx above. It returns the
## inverse of a cache matrix. However, it only (re)calculates the inverse if
## it is not cached already i.e. if the cached inverrse is NA. When it does
## this, it also sets the inverse so that future calls to cacheSolve will
## used the cached inverse unless the matrix has been re-set.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if (is.na(i)) {
                i <- solve(x$get())
                x$setinverse(i)
        }
        i
}