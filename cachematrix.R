## This file contains the two functions required for Programming Assignment 2
## 1. makeCacheMatrix
## 2. cacheSolve

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## s should contain the inverse of the matrix  
  s <- NULL
  
  set <- function(y)  {
    x <<- y 
    s <<- NULL
  }
  
  ## return the original matrix
  get <- function() x
  
  setinverse <- function(inverse) s <<- inverse
  getinverse <- function() s
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of a special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), then
## cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Using the solve(x) function is recommended
        s <- x$getinverse()
                
        if(!is.null(s)) {
          ## return the cached inverse
            message("getting cached data")
            return(s)
        } else {
          ## compute inverse
          i <- solve(x$get())
          ## save inverse to cache
          x$setinverse(i)
          message("inverse calculated")
          ## display inverse
          return(i)
        }
}
