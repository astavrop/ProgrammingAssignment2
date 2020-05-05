## Put comments here that give an overall description of what your
## functions do

#use inv from library matlib to calculate the inverse 
library(matlib)

## The function makeCacheMatrix gets a matrix as argument. 
##Stores the matrix and its inverse as variables 
##so that can be accessed from the global environment after 
##function executes. 

makeCacheMatrix <- function(x = matrix()) {
#I variable stores the inverse of the matrix
I<-NULL
set <- function(y) {
  x <<- y
  I <<- NULL
}
get <- function() x
setInverse <- function(inv) I <<-inv
getInverse <- function() I
list(set = set, get = get,
     setInverse = setInverse,
     getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- inv(data, ...)
  x$setInverse(m)
  m
}
