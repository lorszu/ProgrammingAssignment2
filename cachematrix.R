## Calculation of a inverse matrix with the help
## of caching to reduce the costly computational costs
## based on two functions (makeCacheMatrix() and cacheSolve())


## function makeCacheMatrix()

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
## set and get the matrix that should be inverted
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
## using the solve-function to set and get the inverse matrix
	setinverse <- function(solve) m <<- solve
	getinverse <- function() m
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## function cacheSolve() retrieves the inverse matrix 
## that is already cached in function makeCacheMatrix()

cacheSolve <- function(x, ...) {
	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached matrix")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	
## Return a matrix that is the inverse of 'x'
	return(m)
}
