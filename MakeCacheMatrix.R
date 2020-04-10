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

