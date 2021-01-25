## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function.
#This function crates a matrix that can save it´s inverse. 
#Wen you use the cacheSolve function, the inverse matrix will be saved.
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(inverse) {inv <<- inverse}
	getinv <- function() {inv}
	list(set = set, get = get,
	     setinv = setinv,
	     getinv= getinv)
}



## Write a short comment describing this function
#This function returns the inverse of a matrix
#In case it has been computed before, it returns the inverse matrix saven in cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	inv
}
