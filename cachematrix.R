################################################################
## tested on:

##D=matrix(c(3,2,6,4,8,6,9,0,3),nrow=3, ncol=3)

##testM <- makeCacheMatrix(D)
##cacheSolve(testM)

################################################################

## Function which is able to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}


## Function which computes matrix inverse. If data cached previously, it obtain matrix inverse from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
