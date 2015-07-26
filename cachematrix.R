## The first function, makeCacheMatrix creates a "special" matrix.
## The second function returns the inverse of the "special" matrix.
## set the value of the matrix.
## Jvalvo coursera rprog-030, July 26, 2015
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        ## set the value of the matrix.
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## get the value of the matrix.
        get <- function() x
        ## set the value of the inverse matrix using solve function.
        setinverse <- function(solve) m <<- solve 
        ## get the value of the inverse matrix.
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        ## if the inverse matrix already exists this retrieves the inverse matrix. 
        if(!is.null(m)) {
                ## sends user the message "getting cached data".
                message("getting cached data")
                return(m)
        }
        ## Otherwise, it calculates the inverse of the matrix and sets the value of the   
        ## inverse matrix in the cache via the setinverse function.
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
