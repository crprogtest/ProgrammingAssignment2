## Put comments here that give an overall description of what your
## functions do

## Functions that from matrix creates list with environmnet variables so it can be uses to store cached
## matrix and to extract if cache is already computed

makeCacheMatrix <- function(m = matrix()) {
        i <- NULL
        setM <- function(n) {
          m <<- n
          i <<- NULL
        }
        getM <- function() m
        
        setInverse <- function(inverse) i <<- inverse
        
        getInverse <- function() i
        
        list(setM = setM, getM = getM, setInverse = setInverse, getInverse = getInverse)
}


##Functions that checks if cache of inverstible matrix exists. If not exists, it computes new
# If exists it returns from cache

cacheSolve <- function(m, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- m$getInverse()
        
        if(!is.null(i)) {
          message("getting cached data")
          return(i)
        }
        
        data <- m$getM()
        i <- solve(data, ...)
        m$setInverse(i)
        i
}
