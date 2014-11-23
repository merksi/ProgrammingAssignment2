## These two functions are part of the second programming assignment
## for the R programming language course. 
## The functions allow the user to create a matrix object which can cache its inverse

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    # initialize inverse to Null
    inverse <- NULL
    # for new data, set the x to the matrix 
    # and initialize inverse to NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    # retrieve the matrix
    get <- function() x
    # calculate the inverse of the matrix 
    setinverse <- function(solve) inverse <<- solve
    # get the matrix 
    getinverse <- function() inverse
    # 
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    # if retrieved data is not Null, retrieve cached value
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    # otherwise calculate inverse
    data <- x$get()
    inverse <- solve(data, ...)
    # set inverse in object
    x$setinverse(inverse)
    # return inverse matrix
    inverse
}
