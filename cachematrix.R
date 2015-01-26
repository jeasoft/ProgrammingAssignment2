## author: Jearel Alcantara <jeasoft@gmail.com>

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

    f <- NULL

    set <- function(y) {
        x <<- y
        f <<- NULL
    }

    get <- function() f
    
    setInverse <- function(inverse) f<<- inverse
    
    getInverse <- function() f

    list(set = set, get = get,
    	setInverse = setInverse,
    	getInverse = getInverse
    )
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

    f <- x$getInverse()

    if(!is.null(f)) {
        message("getting cached data")
        return(f)
    }

    data <- x$get()

    f <- inverse(data, ...)

    x$setInverse(f)

    f
}