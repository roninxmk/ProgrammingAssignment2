##-------------------------------------------------------------------------------------------------
## makeCacheMatrix  :   creates a special matrix object which inverse can be cached. 
##                      The matrix is assumed to be always invertible
##                      The methods in this function are:
##                      - get(): gets the value of the matrix
##                      - set(): sets the value of the matrix
##                      - getinv(): gets the inverse of the matrix
##                      - setinv(): sets the inverse of the matrix
##
##-------------------------------------------------------------------------------------------------

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}

##-------------------------------------------------------------------------------------------------
## cacheSolve       :   returns the inverse of the matrix created by 'makeCacheMatrix' function.
##                      If the matrix didn't change and its inverse is already calculated,
##                      then 'cacheSolve' retrieves the inverse from cache. Otherwise, 'cacheSolve'
##                      needs to calculate the inverse of the matrix.
##          
##                      This function uses the makeCacheMatrix$getinv() method to check if the 
##                      inverse of the matrix was calculated. 
##                      If the returned value of such method is NULL:
##                          - then the function gathers the matrix using the makeCacheMatrix$get()
##                          method, calculates the inverse of the matrix and stores it using the
##                          makeCacheMatrix$setinv() method.
##                      If the returned value of such method is NOT NULL:
##                          - then the function prints the message "getting cached data" and
##                          just returns the object m (the inverse of matrix)
##
##-------------------------------------------------------------------------------------------------

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
