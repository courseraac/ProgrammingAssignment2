## Put comments here that give an overall description of what your
## functions do

## makecachematrix makes a matrix object that can cache the inverse of a matrix, and is a list
## set the value of a matrix, get the value, set the value of the inverse, and get the 
## value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list (set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## cachesolve solves the inverse of the matrix, if it has not been solved before. If it 
## has previously been solved, the inverse gets cached and it does not re-perform the
## computation

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
