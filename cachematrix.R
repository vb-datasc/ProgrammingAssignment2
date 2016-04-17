## Programming Assignment 2: This R code contains functions to cache the 
## time-consuming 'matrix inverse' computations. This is achieved by creating 2
## functions makeCacheMatrix and cacheSolve. 'cacheSolve' works on the special 
## matrix object created by makeCacheMatrix() and hence needs to be called
## atleast once. 

## Example usage: 
## > x = matrix(data = c(2,0, 0,1), nrow = 2, ncol = 2)
## > cm1 <- makeCacheMatrix(x)
## > cacheSolve(cm1)


## makeCacheMatrix() creates a special list containing functions to 1) set and 
## 2) retrieve the matrix values, and 3) set and 4) retrieve the matrix inverse
## values

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL                 # initialize inv to null 
    
    set <- function(y){         # set the matrix data
        x <<- y
        inv <- NULL
    }
    
    get <- function() x         # get the matrix data
    
    setinv <- function(inverse) inv<<-inverse # set inv to value passed
    
    getinv <- function() inv    # get the inv value
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve checks returns the already computed inverse, if it exists, other-
## wise computes the inverse using function solve()

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of special matrix 'x' 
    inv <- x$getinv()           # get the inv from special matrix
    
    if(!is.null(inv)){          # if the inverse exists, return it
        message("getting cached data")
        return(inv)
    }
        
    data <- x$get()             # get the matrix data
    
    inv <- solve(data, ...)     # solve for inverse
    
    x$setinv(inv)               # set the inverse (caching)
    
    inv                         # return the inverse

}
