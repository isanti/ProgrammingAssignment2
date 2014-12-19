#####R coursera December 2014#####
#Programming Assignment 2#
## The function makeCacheMatrix creates a special "matrix" object that 
##can cache (in a way, store) its inverse.
## Function cacheSolve is able to return the inverse matrix of x or retrieve it 
##if it is already calculated
########the following functions only refer to square invertible matrices########

#makeCachematrix creates a special "matrix" object that can cache its inverse.
#In fact, it creates a list that contains a function to:
    #set the value of the matrix
    #get the value of the matrix
    #set the value of the inverse matrix
    #get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<-y
        inv<<-NULL
    }
    
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function () inv
    list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}


## cacheSolve function calculates the inverse of the special "matrix" created 
#with function makeCacheMatrix. It first checks to see if the inverse has 
#already been calculated. If so, it gets it from the cache (of function makeCacheMatrix) 
#and skips the computation. Otherwise, it calculates the inverse matrix 
#and sets the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
    inv <- x$getinv() 
    
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
###########rprog-016###########
