## Once this file 'cacheMatrix.R' is sourced, the functions makeCacheMatrix() 
## and cacheSolve() will be defined in the Global environment. They enable a
## working cache for the solve() function which returns the inverse of a matrix

## makeCacheMatrix() function constructs a 'class' of sorts for its matrix
## parameter. We will be able to call the 4 functions defined here through
## x$getinverse(), x$set etc. Cache 'm', defined in the makeCacheMatrix
## environment, is emptied when a new matrix is created with this function.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL                       ## represents the contents of the 'cache'
                                    ## within this makeCacheMatrix environment
    set <- function(par) {
        x <<- par
        m <<- NULL                  ## since we're setting a new value for x,
                                    ## empty the cache as its contents (the
                                    ## inverse of x) are no longer valid
    }
    get <- function() x             ## just return the value of the matrix x
    setinverse <- function(inv) m <<- inv 
                                    ## called only from within cacheSolve(),
                                    ## commits to the cache the output of the 
                                    ## solve() function to get the matrix's 
                                    ## inverse
    getinverse <- function() m      ## return the contents of the cache, which
                                    ## could be blank
    list(set = set, get = get,      ## return these 4 functions as the output
         setinverse = setinverse,   ## of the constructor function
         getinverse = getinverse)
}


## cacheSolve() function first checks to see whether we've already stored the
## results of the (potentially time-consuming) inverse calculation for the 
## argument matrix; if so then simply return the cached results, otherwise 
## calculate using the solve() function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()     ## first check the contents of the cache ('m')
                                ## to see if the inverse has already been run.
        if(!is.null(m)) {       ## if 'm' isn't empty then return it - job done
            message("getting cached inverse")
            return(m)
        }
        data <- x$get()         ## otherwise run the solve() function to get
        m <- solve(data, ...)   ## the inverse of the matrix argument, as we
        x$setinverse(m)         ## normally would, and set the cache for
        m                       ## future use
}
