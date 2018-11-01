## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix creates a list containing the function that sets the value of the matrix, we get the value of the matrix, the value of the inverse matrix, we get the value of the inverse matrix

makeCacheMatrix <- function(a = matrix()) {
b <- NULL	
get <- function() return(a)	
getInverse <- function() return(b)	
set <- function(v) {
	a <<- v
	inverse <<- NULL
}	
setInverse <- function(v) b <<- v	
return(
	list(
		set = set,get = get,setInverse = setInverse,getInverse = getInverse
		)	
	)
}


## Write a short comment describing this function
## The function returns the inverse matrix. First it checks if the inversion has already been calculated. If so, it gets the result and skips the calculation. If not, it calculates the reverse.

cacheSolve <- function(cs, ...) {
b <- cs$getInverse()	
if (!is.null(b)) 
return(b)	
a <- cs$get()
b <- solve(a, ...)
cs$setInverse(b)	
	return(b)
}
