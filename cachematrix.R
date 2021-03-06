## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {       # make matrix
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setSolve <- function(solve) inv <<- solve  #calculate inverse matrix
    getSolve <- function() inv                 #cache matrix
    list(set = set, get = get,
         setSolve = setSolve,               
         getSolve = getSolve)
}


## Write a short comment describing this function.  makeCacheMatrix generates a matrix, inverts and chaches it

cacheSolve <- function(x, ...) {
    inv <- x$getSolve()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setSolve(inv)
    inv
}


