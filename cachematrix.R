## Two functions to fulfill the second assignment 
## in the R programming course.
## Everything is very much similar to the example
## which caches a mean for a vector

## The makeCacheMatrix function prepares a set 
## of functions and returns them to the parent
## environment
## The function creates a special matrix that
## can have its inverted value cached

## Initializing objects x and invm, both
## withing makeCacheMatrix environment

makeCacheMatrix <- function(x = matrix()) {
        invm <- NULL
        
        ## Defining the getters and setters
        set <- function(y) {
                
                ## Assigning the y to the x - object
                ## in the parent environment
                x <<- y
        
                ## Clearing any previous invm value,
                ## so that only the right version of cache is used
                invm <<- NULL
        }
        get <- function() x
        
        ## Assigning input argument to the value
        ## of invm from the parent environment
        setInverted <- function(inverted) invm <<- inverted
        getInverted <- function() invm
        
        ## Returning makeCacheMatrix as a named list
        ## with four objects
        list(set = set, get = get,
             setInverted = setInverted,
             getInverted = getInverted)

}


## The function calculates the inverse of a matrix,
## but if there's already an inverted value cached
## it returns the value right away

## This made possible by either populating or
## retrieving from the makeCacheMatrix

cacheSolve <- function(x, ...) {
        
        ## First, we try retrieving the inverse from cache
        invm <- x$getInverted()
        
        ## If there's a cached value, we return it
        if(!is.null(invm)) {
                message("getting cached inverse of a matrix")
                return(invm)
        }
        ## If there was no cached value, we get the matrix
        ## from the input object
        data <- x$get()
        
        ## Then we calculate the inverted value
        invm <- solve(data, ...)
        
        ## And return this value to the parent environment
        x$setInverted(invm)
        
        ## At last, printing this value
        invm
}
