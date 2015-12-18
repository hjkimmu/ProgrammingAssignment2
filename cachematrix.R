## Write a pair of functions that cache the inverse of a matrix
## The matrix is assumed to be square and invertible

## makeCacheMatrix: Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    # This function returns a list (set the value of the vector, get the value 
    # the vector, set the value of the inverse, and get the value of the inverse)
    )
    
}


## cacheSolve: computes the inverse of the "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated, the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    # Check if the inverse has already been calculate. If so it gets the inverse 
    # from the cache and skips the compuation
    
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
    # Return a matrix that is the inverse of 'x'
}
