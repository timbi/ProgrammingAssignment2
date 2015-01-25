##This function creates a list of functions to store an inverted matrix in memory.  
##  $get: returns the matrix from environment
##
##  e.g.   m <- rbind(c(10,0,10), c(5,1,20), c(10,0,20))
##         mm <- makeCacheMatrix(m)
##         mm$get()
##
##  $setinvert:  to copy inverted matrix to cache in another environment
##  e.g.
##          mm$setinvert(m)
##  
##  $getinvert:  to return from cache the inverted matrix
##  e.g.
##          mm$getinvert()

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinvert <- function(solve) m <<- solve
    getinvert <- function() m
    list(set = set, get = get,
         setinvert = setinvert,
         getinvert = getinvert)
}


##This function returns inverted matrix from memory.  It requires the makecacheMatrix function as a parameter.  
##It also checks to see if the inverted matrix already exists.  If it does it will get it from cache.
##  e.g.   cacheSolve(mm)


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinvert()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinvert(m)
    m
}
