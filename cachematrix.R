## This function creates a matrix object.

makeCacheMatrix <- function(x = matrix()) { ## I'm defining the argument with default mode of "matrix".
    Inv <- NULL                             ## I initialize 'Inv' as NULL. 
    set <- function(y) {                    ## I define the set function to assign new 
        x <<- y                             ## value of matrix in parent environment
        Inv <<- NULL                        ## if there is a new matrix, reset 'Inv' to NULL.
    }
    get <- function() x                     ## I define the get fuction - it returns the value of the matrix argument.

    setinverse <- function(inverse) Inv <<- inverse  ## It assigns value of 'Inv' in parent environment.
    getinverse <- function() Inv                     ## It gets the value of 'Inv' where called.
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ## I use this to refer 
                                                                                
}

## This function computes the inverse of the matrix returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## This function returnes a matrix that is the inverse of 'x'
    Inv <- x$getinverse()
    if(!is.null(Inv)) {
        message("getting cached data")
        return(Inv)
    }
    data <- x$get()
    Inv <- solve(data, ...)
    x$setinverse(Inv)
    Inv
}
