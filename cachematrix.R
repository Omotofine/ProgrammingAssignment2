######################Caching the Inverse of a Matrix##########################
##These pairs of functions caches the inverse of a matrix
###Uses the following functions:
###1. makeCacheMatrix: This function creates a special "matrix" object that
###can cache its inverse.
###2. cacheSolve: This function computes the inverse of the special "matrix" 
###returned by makeCacheMatrix above. If the inverse has already been calculated
###(and the matrix has not changed), then the cachesolve should retrieve the 
###inverse from the cache.


## This function will return a list used to
## set and get the matrix
## set and get the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function () x
    set_inverse <- function(solve) m  <<- solve
    get_inverse <- function() m
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}



##The following function calculates the inverse of the special "matrix" 
##created above however, it first checks to see if the solve has already
##been calculated. If so, it gets the inverse from the cahe and skips the
##computation otherwise, it calculates the inverse of the data and sets the 
##value of the inverse in the cache via the set_inverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$get_inverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$set_inverse(m)
    m
}

