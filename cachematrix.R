## The following functions inverse any given inversable matrix
## If the matrix provided is not inversable, this will result in error
## Call the cacheSolve by passing as arguments the makeCacheMatrix and the matrix to be inversed 

## This function inverses the matrix and caches the inversed result

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    inversematrix <- function(solve) m <<- solve
    getinversematrix <- function() m
    list( set = set, 
          get = get,
          inversematrix = inversematrix,
          getinversematrix = getinversematrix )
}


## This function calls the makeCacheMatrix.
## If the matrix is already cached, then the function will not inverse it again

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
    m <- x$getinversematrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$inversematrix(m)
    m
}
