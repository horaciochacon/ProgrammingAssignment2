## Programming Assignment 2: Lexical Scoping
## Creator: Horacio Chacón Torrico
## These two functions together read a matrix object, computes the 
## matrix inverse, display it and stores the result. If executed again the CacheSolve
## function with no change, the cached inverse is displayed, otherwise
## a new solve() funcition with new input is processed

## This function reads a matrix object and return a list with 4 different
## functions to set and get the matrix and to set and get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function takes the instantiated MakeCacheMatrix function as input and
## tries to calculate the inverse matrix to the previosly setted matrix in
## the MakeCacheMatrix object. If inverse previously existed, cached result is 
## displayed.

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("Printing cached inverse matrix")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}


