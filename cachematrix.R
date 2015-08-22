## Functions for creating a matrix and handling a cached version of its inverse
## 

## Creates the matrix object with its attributes and methods (OO-like).

makeCacheMatrix <- function(x = matrix()) 
{
    theInverse <- NULL
    set <- function(y) {
        x <<- y
        theInverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) theInverse <<- inv
    getInverse <- function() theInverse
    list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## Uses the cached version of the matrix and or set it if the cached version is not available, yet.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    res <- x$getInverse()
    if(!is.null(res)) {
        message("returning cached inverse")
        return(res)
    }
    mat <- x$get()
    matInv <- solve(mat, ...)
    x$setInverse(matInv)
    matInv
}

