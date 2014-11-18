## makeCacheMatrix and cacheSolve are used to cache the inverse of a matrix, to read the cache if the matrix hasn't changed instead of calculating its value once more.

## makeCacheMatrix is a function which takes a matrix as argument, and provides functions to set/get its value and its inverse.
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## cacheSolve is a function which takes a makeCacheMatrix function, and returns its inverse.
## The ... are used to pass additional arguments to the solve() function.
## If its inverse has already been calculated, get its value, else compute it and stock it in the cache.
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
