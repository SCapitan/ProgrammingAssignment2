rm(list = ls())

## makeCacheMatrix: This function creates a special 
## "matrix" object that can cache its inverse.
## I tweaked the meanCache fxn

makeCacheMatrix <- function(x = matrix()) { 
        w <- NULL # sets w to null to initialize for later code
        set <- function(y) {
                x <<- y # assigne y to x in parent environment
                w <<- NULL # assign NULL to the w in the parent environment
        }
        get <- function() x # returns the matrix
        setInverse <- function(inverse) w <<- inverse
        getInverse <- function() w  # get the inverse w
        # assign set/get to the set()/get()
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
}


## cacheSolve: This function computes the inverse of 
## the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        w <- x$getInverse() # assign inverse to W
                if(!is.null(w)) { # if w is not null, return w and print message
                        message("getting cached data")
                        return(w)
                }
                data <- x$get()
                w <- solve(data, ...) #solve(X) returns its inverse
                x$setInverse(w)
                w ## return matrix that is the inverse of 'x'
}

