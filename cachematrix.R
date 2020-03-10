##This function is used to cache the inverse of matrix so that 
##you don't have to calculate again next time.

##This first function creates a list containing a function to set & get the value of the matrix
##and set & get the value of the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
	    inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, 
             getinverse = getinverse)

}


## This function is used to calculate the matrix inverse.

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
