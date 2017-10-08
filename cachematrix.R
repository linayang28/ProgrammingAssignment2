## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# makeCacheMatrix creates a list containing a function to
makeCacheMatrix <- function(x = matrix()) {
                inversion <- NULL
                # 1. set the value of the matrix
                set <- function(y) {
                        x <<- y
                        inversion <<- NULL
                }
                
                # 2. get the value of the matrix
                get <- function() x
                
                # 3. set the value of inverse of the matrix
                setinverse <- function(inverse) inversion <<- inverse
                
                # 4. get the value of inverse of the matrix
                getinverse <- function() inversion
                
                list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## Write a short comment describing this function
## The following function calculates the inverse of the special "vector" created 
## with the above function. However, it first checks to see if the inverse has 
## already been calculated. If so, it gets the inverse value from the cache 
## and skips the computation. Otherwise, it calculates the inverse of the data and 
## sets the value of the inverse value in the cache via the setinverse function.

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
