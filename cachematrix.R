## This R code is for Programming Assignment 2
## Cache time consuming computations like inverse of a matrix


## This function returns a list of setter and getter methods.
## The objects x and m are also siblings of the setter and getter methods
## x is the matrix (input) and m is the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(inv) m <<- inv
	getinverse <- function() m
	list(set = set, get = get,
	     setinverse = setinverse,
	     getinverse = getinverse)
}


## This function finds the inverse of the matrix created in the above
## makeCacheMatrix function.  The function checks if the inverse is already
## evaluated.  If not evaluated, it uses the R's solve function to get the
## inverse

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
