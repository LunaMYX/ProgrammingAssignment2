## These functions basically return the inverse of a matrix and cache it.

## This function creates a special "matrix" that cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y){
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

## This function returns the inverse of matrix x, either by computing it or retrieving it from the function above.

cacheSolve <- function(x, ...) {
	i <- x$getinverse()
   if(!is.null(i)) {
           message("getting cached data")
           return(i)
   }
   i <- solve(x$get(), ...)
   x$setinverse(i) ##Cache the inverse of martix x into the special "matrix"
   i ## Return a matrix that is the inverse of 'x'
}


## Example:
## x<-matrix(c(1,2,3,4),2,2)
## y<-makeCacheMatrix(x)
## cacheSolve(y) # Return the inverse of x and cache the inverse of x into y