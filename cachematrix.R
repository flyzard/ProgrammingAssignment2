## The following functions are an exercise to introduce the operator `<<-`
## which assigns the value to an object in a environment different from
## the current environment. 

## makeCacheMatrix function returns a list representing an object with 
## the several methods to control values of the inner properties

makeCacheMatrix <- function(x = matrix()) {
  	i <- NULL
  	set <- function(y) {
  		message("setting cached data")
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


## In cacheSolve function gets a makeCacheMatrix list and checks if the
## inverse matrix value is already calculated in the give list object. 
## In case this is already calculated, returns the value, if not, 
## gets the data from the given matrix, inverts it, and call 
## setinverse on the list object to store the value in x.

cacheSolve <- function(x, ...) {
  	i <- x$getinverse()
  	if(!is.null(i)) {
		message("getting cached data")
		return(i)
  	}
  	data <- x$get()
  	i <- solve(data)
  	x$setinverse(i)
  	i
}
