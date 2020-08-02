## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix and cacheSolve are functions used in conjunction
## with matrices. Calculating the inverse of a matrix is computationally
## taxing on the software, and these two functions can be used to cache
## the inverse matrix. This way, should we need the inverse matrix we can
## simply call for it, rather than solve for it once again.


## Write a short comment describing this function

## makeCacheMatrix is used to initialize the matrix we would like
## to store the inverse of. When a variable is assigned to this function,
## that variable actually becomes a list of 4 functions: set, get, setinv
## and getinv. 

makeCacheMatrix <- function(x = matrix()) {
        mi <- NULL
        set <- function(y){
                x <<- y
                mi <<- NULL
        }
        get <- function()x
        setinv <- function(inv) mi <<- inv
        getinv <- function() mi
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

## cacheSolve checks to see if the inverse of the matrix has already been
## solved. If not, it solves for it and adds it to the "setinv" 
## component of the input. At the end, it returns the matrix inverse.
## The function also lets the user know when it is using a cached 
## inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mi <- x$getinv()
        if(!is.null(mi)){
                message("getting cached inverse")
                return(mi)
        }
        data <- x$get()
        mi <- solve(data)
        x$setinv(mi)
        mi
}
