## Implementation of a cached matrix that can store and retrieve
##  its own inverse as called by a specialty function that will
#   calculate and set this matrix's inverse if it hasn't been set yet
#   and simply retrieve it if it has.

## makeCacheMatrix is a wrapper function around a matrix. It has internal
#   functions that can be called to set and get the matrix as well as its
#   inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
} 


## cacheSolve accepts an object of type makeCacheMatrix 
##  It looks to see if this object already has an inverse stored
##  and if so retrieves it. If not, it will calculate and store
##  the result in the makeCacheMatrix object for later retrieval

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}