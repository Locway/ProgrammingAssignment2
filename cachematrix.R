## Put comments here that give an overall description of what your
## functions do
# the functions below will read and invert a matrix, placing any solved matrix into cache so that it need not be 
# resolved everytime it is needed

## Write a short comment describing this function
# The makeCacheMatrix function below sets up and intialises a matrix in cache, it also creates set, get, 
# setInverted and getInverted functions for use

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
# cacheSolve will test if the matrix inversion has already been solved, if yes then it returns this from cache
# otherwise it will solve for the inverse

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
