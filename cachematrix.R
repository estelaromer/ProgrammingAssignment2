## These two functions cache the inverse of a matrix
## It is assumed that the matrix supplied is always
## invertible.

## The following function creates a special 
## "matrix" object that can cache its inverse.
## This special "matrix" is actually a list containing
## a function to: 1. set the value of the matrix, 2. get
## the value of the matrix, 3. set the value of the inverse
##matrix and 4. get the value of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function (y) {
		x <<- y
		inverse <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) inverse <<- solve
	getinverse <- function() inverse
	list(set = set, get = get, 
	setinverse = setinverse, getinverse = getinverse)

}

## The following function calculates the mean of the special 
## "matrix" created with the above function. However, it first 
## checks to see if the mean has already been calculated. If so, 
## it gets the inverse matrix from the cache and skips the 
## computation. Otherwise, it calculates the inverse matrix of 
## the data and sets the value of the inverse matrix in the cache 
## via the setinverse function.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	inverse <- x$getinverse()
	
	if(!is.null(inverse)){
		message("getting cached data")
		return(inverse)
	}
	data <- x$get()
	inverse <- solve(data, ...)
	x$setinverse(inverse)
	inverse
}


