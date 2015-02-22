## These functions allow to cache the computed inverse of a matrix in order to avoid repeated
## computations of the same inverse matrix

## This function creates a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function () x
	setsolve <- function(solve) i <<- solve
	getsolve <- function () i
	list (set = set, get = get,
	setsolve = setsolve,
	getsolve = getsolve)
}


## This function computes the inverse of the matrix returned by makeCacheMatrix 
## If the inverse has already been calculated for the matrix the inverse is retrieved from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getsolve()
        if(!is.null(i)) {
        	message("getting cached data")
        	return(i)
        	}
        	data <- x$get()
        i <- solve(data, ...)
        	x$setsolve(i)
        i
}
