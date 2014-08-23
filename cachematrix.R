## Pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(m = matrix()) {

        ## We initialize the inverse matrix
        i <- NULL

        ## Sets the matrix
        set <- function( matrix ) {
                m <<- matrix
                i <<- NULL
        }

        ## Gets the matrix
        get <- function() m

        ## Sets the inverse matrix
        setInverse <- function(inverse) i <<- inverse

        ## Gets the inverse matrix
        getInverse <- function() i

        ## Returns a list of the methods
        list(set = set, get = get,
                setInverse = setInverse,
                getInverse = getInverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    
        ## Extract the inverse matrix from cache
        m <- x$getInverse()

        ## Only return it if it has already been calculates
        if( !is.null(m) ) {
                message("getting cached data")
                return(m)
        }  

        ## Otherwise, recover the original matrix...
        data <- x$get()

        ## And calculate its inverse
        m <- solve(data) %*% data

        ## Set the inverse matrix to the object
        x$setInverse(m)

        ## Return the matrix
        m
}
