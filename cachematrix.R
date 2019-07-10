## These two functions solve a matrix and cache the inverse to be 
## retrieved later rather than calculated over again

## This function returns a list of helper functions to set the value
## of a matrix, get its value, set the value of the inverse, and get
## the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	setMatrix <- function(y){
		x <<- y
		inv <<- NULL
	}
	getMatrix <- function() x
	setInverse <- function(inverse) inv <<- inverse
	getInverse <- function() inv
	list(setMatrix = setMatrix, getMatrix = getMatrix, 
		setInverse = setInverse, getInverse = getInverse)
}


## This function checks whether a matrix has already been solved.
## If so, it returns the value of the inverse.  If not, it solves the
## matrix and caches the value for later retrieval.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        
	##Check whether an inverse value exists
	if(!is.null(inv)){
        	message("getting cached data")
        	return(inv)
        }
	
        data <- x$getMatrix()
        
	##solve for the matrix inverse if it doesn't exist
	inv <- solve(data, ...)
       
	##set the cache the value of the calculated inverse for later retrieval
	x$setInverse(inv)
        inv
}
