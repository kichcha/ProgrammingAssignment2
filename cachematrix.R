## Following pair of functions compute inverse of a matrix in efficient manner
## Efficiency is acheived by cacheing previously computed inverses
## Call the makeCacheMatrix function first followed by cacheSolve function

## function to create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
	## initialize
        inverse <- NULL

	## define get, set, getinverse, setinverse functions
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse

	## list functions / return
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Return a matrix that is the inverse of 'x'
## Return the inverse from the cache if previously computed and cached
## Otherwise, compute inverse and return
cacheSolve <- function(x, ...) {
	## get inverse of x
        inverse <- x$getinverse()
	## check if inverse is not null
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
	## compute inverse, add to cache and return inverse
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}