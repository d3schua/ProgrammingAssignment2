## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	mat <- NULL
	set <- function(y){
	x << -y
	mat <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) mat <<- inverse
	getInverse <- function() mat
	list(set = set, get = get,
      	setinverse = setInverse,
      	getinverse = getInverse)	
	}

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
	mat <- x$getInverse
	if( !is.null(mat) ) {
          message("getting cached data")
          return(mat)
	}
	data <- x$get()
	mat <- solve(data) %*% data
	x$setInverse(mat)
	mat
}
