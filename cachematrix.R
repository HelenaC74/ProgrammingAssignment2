## The two functions below create a special object
## that stores a matrix and can cache its inverse.


## The following function creates a special "matrix",
## which is really a list containing a function to 
## - set and get the matrix
## - set and get the inverse of that matrix

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) inverse <<- solve
	getinverse <- function() inverse
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## The following function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above.
## If the inverse has already been calculated and the matrix has not changed,
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## The parameter 'x' is the object returned by makeCacheMatrix
	inverse <- x$getinverse()
	if(!is.null(inverse)) {
		message("getting cached data")
		return(inverse)
	}
	data <- x$get()
	inverse <- solve(data,...)
	x$setinverse(inverse)
	inverse
}
