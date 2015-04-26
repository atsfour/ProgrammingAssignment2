## These two functions make a special matrix object and cache the inverse of it.
## 
## Firstly, call makeCacheMatrix(matrix) and make an object.
## example : mat <- makeCacheMatrix(matrix(c(1,3,4,5,6,7,8,9,10), nrow=3, ncol=3))
##
## The function cacheSolve(mat) is called for the first time,
## then computes inverse of matrix and stores the result.
## And if cacheSolve(mat) is called again, then returns cached result.

## This function makes a list of function: set() ,get(), setinverse(), and getinverse(),
## which is called by "cacheSolve" function.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function returns the inverse of "makeCacheMatrix" object.
## If matrix that is not invertible is passed, then it displays an error message and returns NULL.

cacheSolve <- function(x, ...) {
    data <- x$get()
    if(ncol(data) != nrow(data)) {
        message("\'x\' must be square matrix")
        return(NULL)
    }
    if(det(data) == 0) {
        message("\'det(x)\' must not be zero")
        return(NULL)
    }
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
