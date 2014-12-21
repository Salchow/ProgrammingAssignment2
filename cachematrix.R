## Create a square invertible matrix that can cache its inverse. Calculate
## the inverse: get the result from cache if it has been calculated previously.

## This function creates a square invertible matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        ## set the value of a square invertible matrix
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        ## get the value of the square invertible matrix
        get <- function() x
        
        ## set the value of the inverse of the matrix
        setmatrix <- function(solve) m <<- solve
        
        ## get the value of the inverse of the matrix
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## This function computes the inverse of the matrix returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache with the message "getting cached data".
## Else it calculates the inverse of the matrix and set the value in cache.

cacheSolve <- function(x = matrix(), ...) {
        
        ## check if the inverse of the matrix has been calculated
        
        m <- x$getmatrix()
        
        ## if it has been calculated previously, get the result from the cache
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        
        ## else calculate the inverse of the matrix and set the value in cache
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setmatrix(m)
        m
}
