## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix creates a list containing the function that sets the value of the matrix, we get the value of the matrix, the value of the inverse matrix, we get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## Write a short comment describing this function
## The function returns the inverse matrix. First it checks if the inversion has already been calculated. If so, it gets the result and skips the calculation. If not, it calculates the reverse.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}