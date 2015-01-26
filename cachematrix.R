## R Programming, Assignment 2 by Jaakko Puurunen
## Date: Jan-21, 2015

## makeCacheMatrix
##
## parameter sourceMatrix: matrix, assumed invertible
## returns a list with four functions:
##     1. setMatrix: function to assign matrix, checks if matrices
##                   are different before assigning (avoids unnecessary solve call)
##     2. getMatrix: returns current matrix
##     3. setInverse: function to set inverse matrix value to list
##     4. getInverse: returns current inverse matrix

makeCacheMatrix <- function(sourceMatrix = matrix()) {
  mInverse <- NULL
  
  # assign matrix value and reset inverse matrix value
  setMatrix <- function(y) {
    message('setMatrix() called')
    if (!identical(sourceMatrix, y))
    {
      message('Matrix has changed -> setting new matrix and resetting inverse matrix')
      sourceMatrix <<- y
      mInverse <<- NULL # reset inverse matrix value      
    }
  }
  
  # return current matrix
  getMatrix <- function() {
    message('getMatrix() called')
    sourceMatrix
  }

  setInverse <- function(mInv) {
    message('setInverse() called')
    mInverse <<- mInv
  }
  
  getInverse <- function() {
    message('getInverse() called')
    mInverse
  }
  
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve
##
## Returns a matrix that is inverse of matrix stored in x
##
## parameter x: list generated with makeCacheMatrix
##
## Calculates inverse matrix by calling solve if inverse
## matrix value in x is null (which indicates that matrix
## has changed or inverse hasn't been solved at all yet)

cacheSolve <- function(x, ...) {

  mInv <- x$getInverse()
  
  if (is.null(mInv)) # inverse is NULL -> solve inverse
  {
    message('No inverse found, must solve')
    sourceMatrix <- x$getMatrix() # get source matrix
    mInv <- solve(sourceMatrix, ...)
    x$setInverse(mInv) # solve inverse and assign inverse to list
    #print(identical(mInv, x$getInverse()))
  }
  mInv # return inverse

}
