## This two functions cache the inverse of a matrix.

## The first function creates a special "matrix" object that can cache 
## the inverse of the input square matrix x.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinver <- function(solve) i <<- solve
        getinver <- function() i
        matrix(list(set, get, setinver, getinver),2,2)  
}


## The second function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
        i <- x[[4]]()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x[[2]]()
        i <- solve(data, ...)
        x[[3]](i)
        i
}
