## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(sourceMatrix = matrix()) {
  mInverse <- NULL
  
  setMatrix <- function(y) {
    print('setMatrix() called')
    if (!identical(sourceMatrix, y))
    {
      print('Matrix has changed -> setting new matrix and resetting inverse matrix')
      sourceMatrix <<- y
      mInverse <<- NULL # reset inverse matrix value      
    }
  }
  
  getMatrix <- function() {
    print('getMatrix() called')
    sourceMatrix
  }

  setInverse <- function(mInv) {
    print('setInverse() called')
    mInverse <<- mInv
  }
  
  getInverse <- function() {
    print('getInverse() called')
    mInverse
  }
  
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse, getInverse = getInverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  mInv <- x$getInverse()
  
  if (is.null(mInv)) # must solve
  {
    print('No inverse found, must solve')
    sourceMatrix <- x$getMatrix()
    mInv <- solve(sourceMatrix, ...)
    x$setInverse(mInv)
  }
  mInv

}
