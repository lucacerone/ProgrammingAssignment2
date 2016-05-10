## This file contains two functions makeCacheMatrix and cacheSolve
## that are used together to invert a matrix and cache its results
## to save computation times when the same operation has to be repeated multiple
## times.
##
## Example:
## mcm <- makeCacheMatrix(diag(c(1,2,3,4)))
## im <- cacheSolve(mcm) #the first time the result is computed and cached in the mcm environment
## im <- cacheSolve(mcm) #the second time the cached value is returned

## makeCacheMatrix returns a list made of four functions used to assign and retrieve
## a matrix and its inverse.
## Upon assigning a new matrix the cached value (if any) of the inverse matrix is reset.

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setinverse <- function(invmatrix) im <<- invmatrix
  getinverse <- function() im
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  

}


## cacheSolve checks if the inverse matrix is already in the enviroment 
## accessed by the functions stored in the object created by makeCacheMatrix
## If it is, it returns it without computing it again; otherwise it computes
## the inverse, cache the result and returns the inverse matrix.

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
  im <- x$getinverse()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  data <- x$get()
  im <- solve(data)
  x$setinverse(im)
  im
}
