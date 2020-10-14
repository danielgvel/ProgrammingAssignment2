## This script creates two functions:
##
## makeCacheMatrix returns a list containing functions to (1) set 
## matrix, (2) get matrix, (3) set the value of the inverse of the
## matrix, and (4) get the value of the inverse of the matrix. If
## we have a square matrix M of any dimension, we must create an
## object "matrix" with this function in the following way:
## matrix <- makeCacheMatrix(M). (You can choose any name for these
## objects.)
##
## cacheSolve takes the object "matrix" as an argument. 
## First, it checks if the inverse of the matrix has already been 
## calculated and stored in the cache. In that case, it makes no 
## calculations and simply returns the stored value after printing 
## the message "getting cached data". Otherwise, it calculates the 
## inverse and stores it via the setinverse function.



## This function is very similar to makeVector. Basically, we just
## rename the variables to make the code more readable (for instance,
## instead of a variable m that stores the mean, we have a variable
## x_inv that stores the inverse of x).
## Moreover, we also rename "setmean" and "getmean" to "setinverse"
## and "getinverse", respectively. This function only creates
## the special object that we will use as an argument for the
## cacheSolve function.
makeCacheMatrix <- function(x = matrix()) {
    x_inv <- NULL
    set <- function(y) {
        x <<- y
        x_inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) x_inv <<- inverse
    getinverse <- function() x_inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function is very similar to cachemean, but instead of
## the variable m (namely, the mean), we want to compute x_inv
## (the inverse of the matrix x). Therefore, we change the
## line m <- mean(data, ...) for x_inv <- solve(matrix, ...).
##  "..." indicates we can pass more arguments to the solve
## function, although it is not necessary for our purpose.
cacheSolve <- function(x, ...) {
    x_inv <- x$getinverse()
    if(!is.null(x_inv)) {
        message("getting cached data")
        return(x_inv)
    }
    matrix <- x$get()
    x_inv <- solve(matrix, ...)
    x$setinverse(x_inv)
    x_inv
}
