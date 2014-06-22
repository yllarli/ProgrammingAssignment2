## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Very close to makeVector

makeCacheMatrix <- function(x = matrix()) {
  ## Functions:
  ## set - creates matrix from numeric or 
  ## get - gets matrix out
  ## setinverse - puts inverted matrix into cache
  ## getinverse - gets inverted matrix from cache
  minv <- NULL
  # set functions assumes matrix or numeric input
  # numeric input is converted to matrix
  # not square matrix is converted to square matrix
  set <- function(y) {
    getsqrt <- sqrt(length(y))
    x <<- matrix(y, getsqrt, getsqrt)
    minv <<- NULL
  }
  get <- function() x
  setinverse <- function(curinv) minv <<- curinv
  getinverse <- function() minv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## Very close to cachemean
## takes inverted matrix from cache, calcultaes if cache is empty

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()

  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  m <- solve(x$get(), ...)
  x$setinverse(m)
  m
}
