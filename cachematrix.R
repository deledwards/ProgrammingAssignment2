# The following function creates a list of functions
# that facilitates storing a matrix value ('x') and that matrix's
# inverse ('cachedInverseOfMatrix'). The computed inverse can be 
# accessed repeatedly eleminating the need to recalulate the 
# inverse, which could be costly to do.
makeCacheMatrix <- function(x = matrix()) {
    cachedInverseOfMatrix <- NULL
    set <- function(m){
        x <<- m
        cachedInverseOfMatrix <- NULL
    }
    get <- function() x
    getinverse <- function() cachedInverseOfMatrix
    setinverse <- function(i) cachedInverseOfMatrix <<- i
    list(set = set,
         get = get,
         getinverse = getinverse,
         setinverse = setinverse)
}


# function that takes a cacheble matrix and either 
# gets the cached value for the matrix's inverse,
# or computes the value and caches it.
# returns the inverse of the matrix
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
