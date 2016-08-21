## MakeCacheMatrix creates a matrix object that can  cache the inverse of the matrix.
#
## This function does four things it sets the value of the matrix, gets the value of the matrix, sets the value of inverse matrix, and gets the value of the inverse matrix.

MakeCacheMatrix <- function(x = matrix()) {
    M <- NULL
    set <- function(y){
           x<<-y
           M<<-NULL
    }
    get<- function() x
    setinverse <- function(solve) M <<-solve
    getinverse <- function() M
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function computes the inverse of the matrix that is computed above. If this inverse has already been computed, then cachesolve will simply retrieve matrix form the cache that lies above.

cacheSolve <- function(x, ...) {
        M <- x$getinverse()
        if(!is.null(M)){
            message("getting cached data")
            return(M)
        }
        data <- x$get()
        M <- solve(data,...)
        x$setinverse(M)
        M
}
