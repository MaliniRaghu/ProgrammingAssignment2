## This program contains two functions written for getting inverse 
## of a matrix. It has two functions makeCacheMatrix and cacheSolve
## with the use of these two functions we are able to cache potentailly
## time consuming computation.
## It also shows how lexical scoping works.

## This function creates a special "matrix" object that can cache its inverse.
## It takes invertible matrix as an input and return a list of functions
## Set , Get, setinverse, getinverse

makeCacheMatrix <- function(x = matrix()) {

  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## cacheSolve function takes the list of function returned by the 
## above function as argument and returns inverse of the matrix
## from the cache if it exists else or calculates inverse using solve function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
