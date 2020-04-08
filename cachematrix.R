## These two functions create a matrix that keeps a cached version of the
##   inverse matrix after the first time it is calculated.  Any changes to 
##   the primary matrix clears the cached value and it must be calculated 
##   again.

## function makeCacheMatrix - Creates a custom matrix type that contains a 
##   list of functions that allow for the value of the matrix to be set or 
##   retrieved, and allows the inverse matrix to also be set and retrieved
## 
##   Contains the functions:
##       set
##       get
##       setinverse
##       getinverse
##
##   Takes a matrix, x, as a parameter
makeCacheMatrix <- function(x = matrix()) {
    # set the default inverse matrix to null
    inverse <- NULL
    
    # function for setting the value of the matrix
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    # function for getting the value of the matrix
    get <- function() {
        x
    }
    
    # function for setting the inverse matrix
    setinverse <- function(invertM) {
        inverse <<- invertM
    }
    
    # function for getting the inverse matrix
    getinverse <- function() {
        inverse
    }
    
    # the list of the created functions
    list(set = set, get = get, setinverse = setinverse, 
         getinverse = getinverse)
}


## function cacheSolve - returns either the cached inverse matrix or 
##   calculates the inverse matrix if there is no cached one available
##
##   Takes a cached matrix, x, as a parameter
cacheSolve <- function(x, ...) {
    # check to see if the inverse matrix is already cached
    invertM <- x$getinverse()
    
    # if the inverse Matrix was cached, return it
    if(!is.null(invertM)) {
        message("Getting cached inverse Matrix.")
        return(invertM)
    }
    
    # otherwise caculate the inverse matrix
    normalM <- x$get()
    invertM <- solve(normalM)
    
    # cache the inverse matrix
    x$setinverse(invertM)
    
    # return the inverse matrix
    invertM
}
