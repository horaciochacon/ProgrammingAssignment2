## Programming Assignment 2: Lexical Scoping
## Creator: Horacio Chacón Torrico
## This two functions together reads a matrix object and computes the 
## matrix inverse and stores the result. If executed again the CacheSolve
## function with no change, the cached inverse is displayed, otherwise
## a new solve() funcition with new input is processed


## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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


