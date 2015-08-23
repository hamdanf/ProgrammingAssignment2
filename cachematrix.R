## Put comments here that give an overall description of what your
## functions do

## This function (makeCasheMatrix) will create a square invertible matrix
## and returns a list of functions to get or set an inverted matrix in cache

makeCacheMatrix <- function(x = matrix()) {
        
        cache <- NULL
        set <- function (y){
                x <<- y
                cache <<- NULL
        }
        
        get <- function() x
        
        setMatrix <- function(inverse) cache <<- inverse
        getInverse <- function() cache 
        
        list(set = set, get = get, setMatrix = setMatrix, getInverse = getInverse)
}


## cacheSolve create the inverse of the matrix created in makeCacheMatrix

cacheSolve <- function(x, ...) {
       
        cache <- x$getInverse()
        if(!is.null(cache)) {
                message("getting cached data")
                
        return(cache)
        
}
        
        data <- x$get()
        cache <- solve(data, ...)
        x$setMatrix(cache)
        cache
}








