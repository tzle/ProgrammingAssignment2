## Put comments here that give an overall description of what your
## functions do
## set()        Assigns a value to the x variable
##              Assigns NULL to the m variable
## get()        Outputs the value stored in the x variable
## setinv()     Inverts the x variable
## getinv()     Outputs the value stored in the m variable
## list()       Stores the functions in a list object, variable

## Write a short comment describing this function
## makeCacheMatrix
##              Accepts a matrix variable
##              Defines 4 separate functions (see above) and 
##              also assigns a value to the m variable

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
#         setmean <- function(mean) m <<- mean
#         getmean <- function() m
        setinv <- function(solve) m <<- solve(x)
        getinv <- function() m

        list(set = set, get = get,  
             setinv = setinv,
             getinv = getinv)           
#              setmean = setmean,
#              getmean = getmean) 
}


## Write a short comment describing this function
## cacheSolve 
##      Accepts an undefined variable and its related variables
##      Returns an inverted matrix either:
##      1. From cached memory, or
##      2. Calculated in cacheSolve()
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
#         m <- x$getmean()
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached")
                return(m)
        }
        data <- x$get()         #if cached value exists
        m <- solve(data)
#         x$setmean(m)
        x$setinv(m)
        m
}
