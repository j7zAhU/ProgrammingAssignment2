## The purpose of these two functions is to save
## computing effort when inverting a matrix. Once 
## inverted and stored in a list, the cached matrix
## can be retrieved without having to run matrix
## inversion again

## This takes a numerical invertible matrix and stores
## it in a list where the values can be set and retrieved

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This takes as input, the list produced by the previous
## function and returns the matrix inverse if it has been 
## previously stored, or it will calculate this inverse
## update it in the list and also output it as a result

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
