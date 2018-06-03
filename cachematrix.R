## R Programming Assignment 2
## Week 3

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


## Computes the inverse of the special "matrix" returned by makeCacheMatrix. If the inverse has already been 
## calculated, then the cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
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
