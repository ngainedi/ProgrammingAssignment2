## Objectives: 
##  1. Compute and return the inverse of a matrix.
##  2. If the inverse has already been computed and the matrix has not changed; 
##     return the pre-computed inverse from cache (without re-doing the computation)
##     else do the computation and return the inverse

## makeCacheMatrix and cacheSolve are the functions that create an 'augmented' 
## matrix object and cache its inverse.

 

## makeCacheMatrix - Create an augmented matrix object
##         that contains functions to store the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  ##check if the input is a valid matrix
  if (!is.matrix(x)) {
    stop ('Invalid matrix input')
  }
  ##Initialize the inverse
  inv_x <- NULL
  
  ##set function to intialize augmented matrix object
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  
  ##get function to retrieve augment matrix object
  get <- function() x
  
  ##setinverse function to set inverse of the matrix
  setinverse <- function(inv) inv_x <<- inv
  
  ##setinverse function to get the inverse of the matrix
  getinverse <- function() inv_x
  
  list (set = set, 
        get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## cacheSolve - Returns the inverse of a 'augmented' matrix from cache (if available) 
## else computes inverse, caches and results the result

cacheSolve <- function(x, ...) {
  ##Retrieve the contents of the augmented matrix object
  mat <- x$get()
  inv <- x$getinverse()
  
  ##if inverse exists and matrix has not changed
  ## If matrix has not changed, matrix product should give identity matrix
  if (!is.null(inv) && sum(mat %*% inv - diag(nrow(x))) == 0) {
        message ("Returning from cache")
        return(inv)
  }
  ## Else compute inverse, set the cache and return the inverse
  else {      
      inv <- solve(mat)
      x$setinverse(inv)
      inv
  }
}
