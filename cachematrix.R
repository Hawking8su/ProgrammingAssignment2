## The following functions can be used to compute and cache the 
## inverse matrix of the input matrix.


## The first function will create a list of functions which will be used
## in the following cacheSolve function.
makeCacheMatrix <- function(x = matrix()) {  # x is a matrix
    inv_x <- NULL
    set <- function(y){
        x <<- y
        inv_x <<- NULL
    }
    get <- function() x
    setInvMat <- function(InvMat) inv_x <<- InvMat
    getInvMat <- function() inv_x
    list(set = set, get = get,
         setInvMat = setInvMat,
         getInvMat = getInvMat)
}


## The second function return the inverse matrix of special "matrix" created 
## by makeCacheMatrix. Specifically, if the inverse matrix has already been cached, 
## it will return the cached one, if not, it will compute and cache the inverse matrix.

cacheSolve <- function(x, ...) {  # x is a function list created by makeCacheMatrix
    inv_x <- x$getInvMat()
    if (!is.null(inv_x)){
        message("getting cached inverse matrix")
        return(inv_x)
    }
    org_mat <- x$get()
    inv_mat <- solve(org_mat, ...)
    x$setInvMat(inv_mat)
    inv_mat
}