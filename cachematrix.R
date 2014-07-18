## If the contents of a matrix are not changing, it may make sense to cache
## the value of the inverse so that when we need it again, it can be looked
## up in the cache rather than recomputed.

## Create a special object that stores a matrix and cache's its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() {x}
        setinverse <- function(inverse) {i <<- inverse}
        getinverse <- function() {i}
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Calculates the inverse of the special "matrix" created with
## 'makeCacheMatrix'. It first checks to see if the inverse has already been
## calculated. If so, it gets the inverse from the cache and skips the 
## computation. Otherwise, it calculates the inverse of the data and sets the
## value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        ## assume that the matrix supplied is always invertible
        i <- solve(x$get(), ...)
        x$setinverse(i)
        i
}
