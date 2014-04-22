## Matrix inversion is usually a costly computation and there may be some benefit to 
## caching the inverse of a matrix rather than compute it repeatedly

## `makeCacheMatrix` function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    x_1 <- NULL
    set <- function(y) {
        x<<-y
        x_1 <<-NULL
    }
    get <- function() x
    
    setInverse <- function(inverseX) x_1 <<-inverseX
    getInverse <- function() x_1
    
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


##  `cacheSolve` function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##  If the inverse has already been calculated (and the matrix has not changed), 
##  then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    x_1 <- x$getInverse()
    if (!is.null(x_1)) {
        message("getting cached data")
        return(x_1)
    }
    
    matrix<- x$get()
    x_1 <- solve(matrix)
    x$setInverse(x_1)
    x_1
}
