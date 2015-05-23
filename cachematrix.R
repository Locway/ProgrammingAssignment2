## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
xinverted <- NULL #prebuilding where the result of the inverted matrix is stored
        set <- function(st) {
                x <<- st
                xinverted <<- NULL
                }
        get <- function() x #gets the input matrix
        setInvert <- function(inv) xinverted <<- inv #sets the inverted matrix
        getInvert <- function() xinverted #returns the inverted matrix
        list(set = set, get = get, setInvert = setInvert, getInvert = getInvert)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        mtx <- x$getInvert() #get the the inverted matrix
        if(!is.null(mtx)) { #test if inverted matrix is already there
                return(mtx) #returns the inverted matrix
                
                }
        mydata <- x$get() #if the inverted matrix is not already there then go get it
        mtx <- solve(mydata)
        x$setInvert(mtx) #set the solution to be the object
        return(mtx) #return the inverted matrix
        ## Return a matrix that is the inverse of 'x'
}
