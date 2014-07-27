##Coursera R Programming Assignment 2 ## MJT ## 2014-07-27
##########################################################
## Overall these functions 
##1. allow a matrix to be passed in; 
##2. calculate the inverse and store the result in cache
##3. retrieve the inverse calculation result from cache, rather than recalculating
###################################################################################
## The role of makeCacheMatrix is:
## 1. to receive a matrix parameter
## 2. handle the functions performing the calcuation
## 3. maintaining the cache

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  
  setinverse <- function(t) 
  {
    message("solving costly calcuation")
    i <<- solve(x)%*%x #invert matrix  calculation
  }
  
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

############################################################################
## The role of cacheSolve is to return the inverse matrix
## - either from the getInverse function when return value is not null (meaning the inverse has already been pre-calculated)
## - or from the setInverse function

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  r = x$setinverse(i)
  r
}
##############################################################################
## Tests to ensure the code works
#c = makeCacheMatrix(rbind(c(21, 3), c(5, 7))) ## create example new matrix
#cacheSolve(c) ## example first call performs calc
#cacheSolve(c) ## example second call uses cache
#d = makeCacheMatrix(rbind(c(1, -5), c(5, 3))) ## create example new matrix
#cacheSolve(d) ## example first call performs calc
#cacheSolve(d) ## example second call uses cache
##############################################################################




