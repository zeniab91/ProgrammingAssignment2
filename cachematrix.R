## Put comments here that give an overall description of what your
## functions do

## create an invertible matrix
## return a list containing functions to set and get the matrix, and to set and get the inverse
## this list is used as the input to the cacheSolve() function
## the cacheSolve() function then computes the inverse of the matrix returned by the makeCacheMatrix function
## if the inverse has been calculated, cacheSolve retrieves the inverse from the cache
## if the inverse has not been calculated, cacheSolve will calculate and set the value in the cache, then print the result



## Write a short comment describing this function

## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {        
        
        c <- NULL
        set <- function(y){
                x <<- y         ## use <<- to assign a value in a different environment
                c <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) c <<- inverse
        getinv <- function() c
        list(set=set,get=get,setinv=setinv,getinv=getinv)

}

## Write a short comment describing this function

## Computes the inverse of the matrix returned by makeCacheMatrix. If the inverse has already been 
## calculated, then the cacheSolve function retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        c <- x$getinv()     
      
        if(!is.null(c)){        ## if the inverse has already been calculated
                message ("getting cached data")         ## print message
                return(c)       ## stop calculation and get cached inverse of 'x'
         }
        
        ## otherwise, calculate the inverse using the get() function
        data <- x$get() 
        c <- solve(data, ...)
        
        
        ## set the value of the inverse in the cache using the setinv() function
        x$setinv(c)
        
        ## print result
        c
}



