## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a matrix object with getter and setter methods for the data
## and the inverse of the data. This allows it all to be stored together and updated/cached.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function
## This function takes in a cacheMatrix object and gets or calculates the inverse 
## of the matrix. It first checks whether the inverse has already been calculated and stored,
## if so it returns the previously calculated cached value. Otherwise it does the 
## calculation and stores it in the object.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}
