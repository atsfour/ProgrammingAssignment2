## 
## firstly, makeCacheMatrix(matrix), which stores 
## mat <- makeCacheMatrix(matrix(1:6, nrow=2, ncol=4))
##
## then cacheSolve(mat) was called for the first time,
## 
##
## cacheSolve(mat) 

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


## This function returns the inverse of "makeCacheMatrix" object
## 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
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
