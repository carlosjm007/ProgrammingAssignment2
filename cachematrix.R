# this function is able to save in cache a matrix and its inverse
# but it isn't able to compute its

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	get <- function(){
		x
	}
	setsolve <- function(inver){
		inv <<- inver
	}
	getsolve <- function(){
		inv
	}
	list(
		set = set,
		get = get,
		setsolve = setsolve,
		getsolve = getsolve
	)
}

# this function can compute the inverse matrix, but if the inverse
# matrix has been computed before, this function takes the result
# from the cache

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	inver <- x$getsolve()
	if(!is.null(inver)){
		message("getting cached data")
		return(inver)
	}
	matriz <- x$get()
	inver <- solve(matriz, ...)
	x$setsolve(inver)
	inver
}

# Sorry about my english