## Coursera - R Programming (2015-Nov)
## Based on the source code provided by Roger D. Peng through
## GitHub repository (https://github.com/rdpeng/ProgrammingAssignment2) and
## on the "cachemean" example by Roger D. Peng.
## 
## Programming assignment #2
## 

## Creates a matrix object with its attributes

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

