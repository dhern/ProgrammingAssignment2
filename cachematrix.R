
# These functions create an object ouf of a matrix M, capable of storing a cached
# copy of the inverse of M

# This exercise amounts to changing some names in the example provided

## Creates the matrix object consisting of 4 functions to set and retrieve
## M and its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y) {
        x <<- y     # the name x is defined in the outer scope. To modify it
                    # the <<- operator is required. This is lexical scoping.
                    # In contrast, in dynamic scoping, set would have access to
                    # the variables belonging to the outer scope.
        inv <<- NULL
    }
    
    get <- function() x     # This is where the object returned by makeCacheMatrix
                            # stores the matrix
    
    setinv <- function(inverse) inv <<- inverse
    
    getinv <- function() inv
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Finds the inverse of M, stores and returns it, if it's not already cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    
    # If cached, get it from the cache
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setinv(inv)
    
    inv
}
