## The following functions leverage abstraction in order to 
## provide a means to cache values for later use.

## The makeCacheMatrix function generates a list of functions as a 
## means to act on an instance of an object.

makeCacheMatrix <- function(MatrixIn = matrix()) {
    MatrixOut <- NULL
    set <- function(y) {
        MatrixIn <<- y
        MatrixOut <<- NULL
    }
    get <- function() {
        MatrixIn
    }
    setInverse <- function(InverseVal) {
        MatrixOut <<- InverseVal
    }
    getInverse <- function() {
        MatrixOut
    }
    list(set = set, 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse
    )
}

## This function will get and return a stored inverse matrix if it exists.  
## If it does not exist, the function will solve for the inverse, store it
## and return it.

cacheSolve <- function(MatrixIn, ...) {
    ## Return a matrix that is the inverse of 'MatrixIn'
    InvMatrix <- MatrixIn$getInverse()
    if(!is.null(InvMatrix)) {
        message("getting cached data")
        return(InvMatrix)
    }
    MatrixData <- MatrixIn$get()

    InvMatrix <- solve(MatrixData)
    MatrixIn$setInverse(InvMatrix)
    InvMatrix
}

