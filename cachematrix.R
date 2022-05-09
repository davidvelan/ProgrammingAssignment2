## The combined functions makeCacheMatrix and cacheSolve are used
## to calculate and store a matrix and its inverse. This can be used
## to avoid the repeated calculation of the inverse of a matrix. 

## The makeCacheMatrix takes a matrix as input and returns a list
## of functions which can be used to set and return the matrix and
## its inverse. The object created by the makeCacheMatrix can be 
## used by the cacheSolve function to either calculate the inverse
## matrix if it has not already been calculateed or return the 
## cached inverse matrix

makeCacheMatrix <- function(x = matrix()) {
      invMatrix <- matrix(, nrow = nrow(x), ncol = ncol(x) )
      
      set <- function(y) {
        x <<- y
        invMatrix <<- matrix(, nrow = nrow(x), ncol = ncol(x) )
      }
      get <- function() x
      setInv <- function(inverseVariable) invMatrix <<- inverseVariable
      getInv <- function() invMatrix
      list(set = set, get = get, 
           setInv = setInv,
           getInv = getInv)
}


## The cacheSolve function takes a makeCacheMatrix object and
## either calculates the inverse of the matrix in the object if 
## it has not be calculated or returns the inverse if it has already
## been calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invCache <- x$getInv()
       
        if (!all(is.na(invCache))) {
            message("getting cached data")
            return(invCache)
        }
        data <- x$get()
        invCache <- solve(data, ...)
        x$setInv(invCache)
        invCache
}
