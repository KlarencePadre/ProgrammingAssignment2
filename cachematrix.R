## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is a function that stores a matrix, and caches it

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
          x <<- y
          m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)
}


## cacheSolve is a function that checks if the inverse of a matrix was already
## previously solved and cached. If yes, it retrieves the data. Otherwise,
## it calculates and sets the inverse of the new function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m  
}

