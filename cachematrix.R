# Author: datavector
# Programming Assignment #2
#Problem Statement:
#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# Contains the  setters and getters for the matrix
# and also for the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <-  function(y){
    x<<- y
    inverse<<-NULL
  }
  get <- function() x
  setInverse <- function(inverseMatrix) inverse<<- inverseMatrix
  getInverse <- function() inverse
  list(set=set,get=get,setinverse=setInverse,getinverse=getInverse)
}

#---------------------------------------------------------------------------------------------------------------
#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
#retrieve the inverse from the cache.
#---------------------------------------------------------------------------------------------------------------
# Checks if the inverse of the matrix has already been computed
# if so prints the cached value 
# if not it computes the inverse of the matrix.
cacheSolve <- function(x, ...) {
      inverseMatrix<-x$getinverse()
      if( !is.null(inverseMatrix)){
        print("Returning the cached inverse matrix")
        return(inverseMatrix)
      }
      inputMatrix<-x$get()
      inverseMatrix<-solve(inputMatrix,...)
      x$setinverse(inverseMatrix)
      inverseMatrix
}

# Test the code
# input<- matrix(c(7,0,-3,2,3,4,1,-1,-2), 3, 3)
# class(input)
# matrix<-makeCacheMatrix(input)
# matrix$get()
# cacheSolve(matrix)
# inv<-cacheSolve(matrix)
