## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#makeCacheMatrix creates a list of four functions:set the matrix and its inverse
# get the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
#cacheSolve finds the inverse of matrix which is accessible by the get function from 
# the list x. If the inverse has been calculated before, it simply returns it otherwise
# it calculates and stores back
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
	if(!is.null(inv)){
		message("getting cached data")
		return(inv)
	}
	mdata <- x$get()
	inv <- solve(mdata)
	x$setinverse(inv)
	inv
}
