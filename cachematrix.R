## Put comments here that give an overall description of what your
## functions do

#use inv from library matlib to calculate the inverse 

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
setInverse <- function(Inv) I <<-Inv
getInverse <- function() I
list(set = set, get = get,
     setInverse = setInverse,
     getInverse = getInverse)
}


## If the reverse of matrix created with makeCacheMatrix
##is already calculated,gets cached data
##otherwise calculates the inverse matrix
##and caches it by setting it by using setInverse
##which is a method of x object which is in global environment

cacheSolve <- function() {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  m
}
