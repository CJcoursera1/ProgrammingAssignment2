## Together, the function in this file provide a method for caching the
## inverse of a matrix to save computation time on future requests for the
## inverse of the matrix.

## makeCacheMatrix takes a matrix and create an interface 
## (in the form of a list of functions) for storing and 
## retrieving the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) 
{
    inv <- NULL
    set <- function(y)
    {
        x <<- y
        inv <<- NULL
    }
    get <- function() xmatr
    setinverse <- function(inverse) inv <<- inverse 
    getinverse <- function() inv
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function returns the inverse of a matrix.
## The first time the function is called the inverse is computed and cached.
## cacheSolve assumes that the matrix is invertible.

cacheSolve <- function(x, ...) 
{
    inv <- x$getinverse()
    if(!is.null(inv))
    {
        message("Getting cached data")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    
    inv
}
