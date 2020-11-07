## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Define 4 functions set(), get(), setinverse(), getinverse()
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse 
        ## Set the inverse matrix to cache
        getinverse <- function() inv
        ## Get the inverse matrix from cache
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

## Calculate the inverse matrix if cache is empty
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)  
        ## Calculate the inverse matrix of 'data' 
        x$setinverse(inv)
        inv
}
