###############################################################
###############################################################
## makeCacheMatrix - passes a matrix argument and returns
## a list of functions that operate on the matrix argument.
## cacheSolve - computes and prints the inverse of the matrix 
## passed to makeCacheMatrix if it is not cached, otherwise 
## just prints the cached copy of the inverse.
###############################################################
###############################################################


## makeCacheMatrix - contains 3 functions
## setinv - caches the matrix inverse
## getinv - returns the matrix inverse
## getmat - returns the input matrix
makeCacheMatrix <- function(mat = matrix()){
    #inmat <- mat
    inv <- NULL
    setinv <- function(inver){
        inv <<- inver
    }
    getinv <- function() inv
    getmat <- function() inmat <<- mat
    list(getmat = getmat, getinv = getinv, setinv = setinv)
}


## cacheSolve - passes as argument the return value of makeCacheMatrix.
## gets the inverse from makeCacheMatrix using getinv(),
## checks if the inverse already exists, if yes then prints the cached value,
## else computes the inverse and calls setinv() from makeCacheMatrix to cache a copy.
## 
cacheSolve <- function(x, ...){
    inv <- x$getinv()
    if (!is.null(inv)){
        print("returning cached inverse of the matrix")
        return(inv)
    }
    inmat <- x$getmat()
    inv <- solve(inmat)
    x$setinv(inv)
    inv
}