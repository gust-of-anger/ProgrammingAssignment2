
## This function stores a list containing the input matrix and information
## if the inverse has already been computed.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<-y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get=get,
         setinv = setinv,
         getinv = getinv)
}


## This function computes the inverse of the matrix in the list returned by
## makeCacheMatrix function. If the inverse has already been calculated, this 
## functions only reads the cached information, avoiding the calculation of the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
