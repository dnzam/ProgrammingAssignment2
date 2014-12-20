## A pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
invmatrix<-NULL
setmatrix<- function (y) {
  x<<-y
  invmatrix<<-NULL
  
}
getmatrix <- function() x
setinvmatrix <- function(inversematrix) invmatrix <<- inversematrix
getinvmatrix <- function() invmatrix
list(setmatrix = setmatrix, getmatrix=getmatrix,
     setinvmatrix = setinvmatrix,
     getinvMatrix = getinvMatrix)
}




## This function computes the inverse of the special "matrix" returned by makeCacheMatrix


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invmatrix <- x$getinvmatrix()
  #check if it is not null
  if (!is.null(invmatrix)){
    message("getting cached data")
    return (invmatrix)
  }
  #if it is null, calculate the inverse
  data <- x$getmatrix()
  inv <- solve(data, ...)
  x$setinvmatrix(inv) 
  inv
}

