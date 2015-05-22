makeCacheMatrix <- function(x = matrix()) {
  s<- NULL
  set <- function(y){ ##set is a function that changes the matrix stored in the main function
    x<<- y ##susbtitutes the matrix x with the matrix y in the makeCacheMatrix function
    s<<- NULL ##restores the value of the inverse to NULL because the old value is not needed anymore, the new inverse will be calculated with the function described below
  }
  get<- function() x
  setinverse<- function(solve) s<<- solve
  getinverse<- function() s ##setinverse and getinverse do not calculate the inverse, but they store the value of the input in a variable "s" into the main function (setinverse) and returns it (getinverse)
  list(set =set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}




cacheSolve <- function(x, ...) {
  s <- x$getinverse()
  if(!is.null(s)) {
    message("getting cached data") ##searches to see if s exists and has been previously stored, and if exists returns s value
  return(s)
  }
  data<- x$get() ##if s (the inverse) does not exist, this line gets the matrix and stores it in data
  s <- solve(data, ...) ## this line calculates the inverse of the matrix
  x$setinverse(s) ##this line stores it in the object generated assigned with makeCacheMatrix
  s ##returns a matrix that is the inverse of 'x'
}
