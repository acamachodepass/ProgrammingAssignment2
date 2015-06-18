## Overall Description:
##
## makeCacheMatrix and cacheSolve functions are meant to
## be used together to store and read in cache memory a matrix
## and its inverse.
## A detailed description of both functions
## are given below.
## Following a use example is given which can be reproduced
## using the commented version of the functoins.
## Example:
## > X = matrix(c(1,2,3,4), nrow=2, ncol=2)
## > M = makeCacheMatrix(X)
## [1] "1. m: NULL"
## [1] "6. list"
## > cacheSolve(M)
## [1] "5. getinverse()"
## [1] "3. get()"
## [1] "4. setinverse(solve)"
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(M)
## [1] "5. getinverse()"
## getting cached matrix
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## >
## Debug version of the function makeCacheMatrix:
## makeCacheMatrix <- function(x = matrix()) {
##   print('1. m: NULL')
##   m <- NULL
##   set <- function(y) {
##     print('2. set: x = y, m = NULL')
##     x <<- y
##     m <<- NULL
##   }
##   get <- function() {
##     print('3. get()')
##     x
##   }
##   setinverse <- function(solve){
##     print('4. setinverse(solve)')
##     m <<- solve
##   }
##   getinverse <- function(){
##     print('5. getinverse()')
##     m
##   }
##   print('6. list')
##   list(set = set, get = get,
##        setinverse = setinverse,
##        getinverse = getinverse)
## }




## makeCacheMatrix:
##
## The function makeCacheMatrix serves two purposes: either
## to calculate and store in cache the inverse of the matrix x, or
## to read from memory and return a previously cached matrix or
## the matrix inverse.  Both the matrix and its inverse are
## stored in cache, and later can be recovered.
## The function makeCacheMatrix receives a square matrix x
## a returns a list of functions for caching the matrix:
## set, get, setinverse, and getinverse.
## The returned list of functions are meant to be used by
## the function cacheSolve which is described below.
## The default value of x is the empty matrix.
## If the default argument of x is used, the matrix
## must be set, using the returned set function.
## The function get can be used to retrieve the matrix x.
## The function setinverse calculates the inverse of x
## using R's function solve.  The function getinverse
## can be used to retrieve the inverse matrix stored in cache.
## The function does not make any assertions on the input x:
## it assumed that x is not NULL and is always invertible.

makeCacheMatrix <- function(x = matrix()) {
  # the <<- operator which can be used to assign a value to an object in an
  # environment that is different from the current environment
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() {
    x
  }
  setinverse <- function(solve){
    m <<- solve
  }
  getinverse <- function(){
    m
  }
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve:
##
## The function cacheSolve uses the function makeCacheMatrix
## to store in cache or retreive from cache memory a previously
## stored inverse of a matrix.
## It expects as input x the object returned by the function
## makeCacheMatrix. The first line retrieves from memory the
## inverse of a matrix.  If the inverse is not NULL, that is
## it was previously stored in cache using makeCacheMatrix,
## a message indicating a "getting cached matrix" is printed
## and the cached inverse is returned.
## Otherwise, the matrix (not its inverse) is read from cache,
## then R's solve function is used directly to calculate its
## inverse, then the inverse m is cached in memory using the
## setinverse function provided by makeCacheMatrix, and
## finally the inverse m is returned.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached matrix")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setinverse(m)
  m
}
