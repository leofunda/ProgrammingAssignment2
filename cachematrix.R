

## FIRST FUNCTION: makeCacheMatrix
## the following function stores a matrix and caches its inverse.
makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL ## initializing inverse matrix
  
  ## this setter function uses the <<- operator to assign a value to an object in an
  ## environment that is different from the current environment
  set <- function(y) {
    x <<- y
    xinv <<- NULL ## it also initialises xinv to null
  }
  
  get <- function() x ## returning the input matrix
  setInv <- function(inv) xinv <<- inv ## setting the inverse matrix
  getInv <- function() xinv # return the inversed matrix
  
  ## this function returns a list that contains the following functions:
  ## set, get, setInv, getInv
  ## this way we can use makeCacheMatrix objects as follows
  ## x <- makeCacheMatrix(testmatrix)
  ## x$set(newmatrix) to change matrix
  ## x$get to get the set matrix
  ## x$setInv to set the inverse matrix
  ## x$getInv to get the inverse matrix
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}




## SECOND FUNCTION: CacheSolve
## the following function evaluates the inverse of the matrix returned by makeCacheMatrix().
## If the matrix has not changed and the inverse has already been calculated the function takes
## the inverse from the cache without recalculating it.
cacheSolve <- function(x, ...) {
  m <- x$getInv() ## getting the inversed matrix. it will be null it has not been calculated
  ## (remember the previous function's first line "xinv <- NULL"
  if(!is.null(m)) { ## if the inversionis not null (it has been calculated), the calculated
    ##inversion is taken from the cache and returned
    message("getting cached data")
    return(m)
  }
  data <- x$get() ## if the inversion has been calculated, the script applies x$get to get the matrix x
  ## and then computes its inverse
  m <- solve(data)
  x$setInv(m) ## setting the inverse matrix
  m
}
