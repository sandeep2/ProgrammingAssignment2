## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix performs following operations: set the value of matrix, get the value of matrix, set inverse value of matrix and get the value of matrix

makeCacheMatrix <- function(x = matrix()) {
    temp <- NULL
    set <- function(y) {
        x <<- y
        temp <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) temp <<- inverse
    getinverse <- function() temp
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
##cacheSolve computes the inverse of the matrix created above. If the inverse is calculated then it should get the inverse from cache.
cacheSolve <- function(x, ...) {
    temp <- x$getinverse()
    if(!is.null(temp)) {
        message("getting cached data")
        return(temp)
    }
    data <- x$get()
    temp <- solve(data)
    x$setinverse(temp)
    temp
}
