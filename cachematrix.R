## The functions allows to create an object that stores a square matrix and
## its inverse in a cache to make computations go faster.

## makeCacheMatrix takes as input a square matrix with
## the determinant that not equals to zero.
## It returns a list of length 4 that contains functions 
## which allow to set a matrix & its inverse and to return them.

makeCacheMatrix <- function(m) {
		inv <- NULL
		if(!isGood(m)){  # check if the inverse matrix can be found.
			return(NULL)}
		set <- function(y) {
			if(isGood(y)) {
				m <<- y
				inv <<- NULL
			}
		}
		get <- function() m
		setinv <- function(inverse) inv <<- inverse
		getinv <- function() inv
		list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve returns the inverse of a matrix.
## Input - special list object created by makeCacheMatrix
## that contains the matrix and its inverse (if matrix wasn't changed by $set method.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	i <- x$getinv()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setinv(i)
	i

}

## The helper function that checks if the input is valid.
## It is used by makeCacheMatrix function to check if the inverse can be computed.

isGood <- function(mx) {
		# The helper function to test if the inverse can be
		# computed from the input matrix.
		if(!is.matrix(mx)) {
			message("The argument is not a matrix!")
			return(F)}
		if(nrow(mx) != ncol(mx)) {
			message("The matrix is not square! Can't find the inverse!")
			return(F)}
		if(is.na(det(mx))) {
			message("Determinant = NA. Can't find the inverse!")
			return(F)}
		if(det(mx) == 0) {
			message("Determinant = 0. Can't find the inverse!")
			return(F)}
		TRUE
}

