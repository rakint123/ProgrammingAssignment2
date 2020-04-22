## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Create function
makeCacheMatrix <- function(x = matrix()) {
        ##Set Matrix and inverse properties
        m <- NULL
        set <- function(matrix) {
                x <<- matrix
                m <<- NULL
        }
        ## Methods to get matrix, set the inverse, and getting the inverse of the matrix
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        ## Returning methods list
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

## Computing a function cacheSolve based  on returns from makeCacheMatrix
cacheSolve <- function(x, ...) {
      ## Returning matrix inverse of x and return inverse if already there
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## Get inverse, calculate, and set the inverse of the matrix and returning it
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}


