##Creating object which can cache inverse matrix 

makeCacheMatrix <- function(x = matrix()) {

    inver <- NULL

    set <- function(y) {
		x <<- y
		inver <<- NULL

    }
	get <- function() x
	setinverse <- function(inverse) inver <<- inverse
	getinverse <- function() inver

    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}

## Computes inverse. If the inverse has already been calculated,then the cachesolve should get the inverse from the cache

cacheSolve <- function(x, ...) {

    inver <- x$getinverse()
	if(!is.null(inver)) {
		return(inver)

    }
	data <- x$get()
	inver <- solve(data)
	x$setinverse(inver)
	inver

}
