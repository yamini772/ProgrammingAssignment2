## These functions written in partial fulfillment of Coursera Data Science: R Programming 
## Week 3 Assignment; week beginning January 18, 2016; GitHub user: PamlaM

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) { ## define the argument with default mode of "matrix"
         inv <- NULL                             ## initialize inv as NULL; will hold value of matrix inverse 
         set <- function(y) {                    ## define the set function to assign new 
                 x <<- y                             ## value of matrix in parent environment
                 inv <<- NULL                        ## if there is a new matrix, reset inv to NULL
 }
 get <- function() x                     ## define the get fucntion - returns value of the matrix argument
        
        setinverse <- function(inverse) inv <<- inverse  ## assigns value of inv in parent environment
         getinverse <- function() inv                     ## gets the value of inv where called
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ## you need this in order to refer 
         ## to the functions with the $ operator

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve will retrieve the inverse from the cache

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
## ---------------Checking the program------------------------
## m <- matrix(rnorm(16),4,4)
## m1 <- makeCacheMatrix(m)
## cacheSolve(m1)

## [,1]       [,2]       [,3]       [,4]
## [1,] -0.1653269  0.2592203  0.6176218 -0.7520955
## [2,]  0.2828334 -0.1853499  0.4511382  0.2094365
## [3,]  0.1434840  1.0413868 -0.3550853 -0.3261154
## [4,]  0.1793583 -0.4252171 -0.4371493 -0.1749830
