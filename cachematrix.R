## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "vector" that
## contains functions to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix inverse
## 4. get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  list(set = set,  get = get,
       setInv = setInv,
       getInv = getInv)
}


## Write a short comment describing this function
## This function calcuates the inverse of the
## special "vector" created with the makeCacheMatrix
## function. It first checks to see if the inverse has
## already been calculated. If so, it gets the inverse
## from the cache and skips the computation. Otherwise,
## it calculates the inverse of the matrix and sets
## the value of the inverse in the cache via the
## setInv function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  if (!is.matrix(mat)) {
    message("input does not contains a matrix")
    return(NULL)
  }
  matDim <- dim(x$get())
  if (matDim[1] != matDim[2]) {
    message("need to be a square matrix")
    return(NULL)
  }
  idMat <- diag(matDim[1])
  inv <- solve(mat, idMat)
  x$setInv(inv)
  inv
}
