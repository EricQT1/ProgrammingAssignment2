## Put comments here that give an overall description of what your
## functions do

## this creates a special 'matrix' object that can cache its inverse
makeCacheMatrix <- function(x=matrix(), ...) {
    i <- NULL
    set <- function(y) {
        x <<- y ## Use '<<' to assign a value to an object in an environment
        m <<- NULL
    }
    get <- function() {
        x
        }
    setM <- function(solve) {
        m <<- solve
    }
    getM <- function() {
        m
    }
    list(set = set,get = get,setM = setM,getM = getM) ## This list returned is used as an input to cacheSolve()
}
 ## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    m <- x$getM() 
    ## if inverse has already been calculated
    if (!is.null(m)) {
         ##get it from the cache and skip the computation.
        message("getting cached data")
        return(m)
    }
     ## Otherwise, calculates the inverse
    data <- x$get()
    m <- solve(data, ...)
    ## sets the value of the Inverse in the cache via setInverse function.
    x$setM(m)
    m
}
