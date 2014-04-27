## So, the idea of this assignment is to write an R function that
## finds the inverse of a matrix, which is a costly computation.
## Btw, im following the same reasoning as in the "Caching the Mean of a Vector" example.


## This function makeCacheMatrix creates a special "matrix" object that can cache its inverse. 
## Therefore, the idea of this function is to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        
        ##to initialize the matrix; I decided to call my inverse matrix c 
        c <- NULL
        
        ## Next, I set the value of the matrix.
        set <- function (y) {                
                x <<- y
                c <<- NULL
        }
        
        ##With this, I get the value of the matrix
        get <- function () x
        
        ## Next, I set the value of the inverse matrix
        setinverse <- function(inverse)   c<<-inverse
        
        ##And finally, I get the value of the inverse matrix
        getinverse <- function() c
        
        list( set=set, get=get,
              setinverse=setinverse,
              getinverse=getinverse)
        
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## As first step, it checks if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips all the computation.
## Otherwise, it calculates the inverse of the data and sets the value in the cache. 


cacheSolve <- function(x, ...) {
       
        ## to get the value of the inverse
        c <- x$getinverse()
        
        ##to check if the inverse is already computed
        if( !is.null(c) ){
                message("getting cached data")
                
                ##If the inverse is cached, R will get the value and return 
                return(c)
        }
        
        
        ##Otherwise, it will calculate the inverse of the data
        data <- x$get()
        c <- solve( data, ...)
        
        ##Now, I set the inverse to the cached value
        x$setinverse(c)
        
        ##And finally I print c, that returns the inverse of x
        c
        
}
