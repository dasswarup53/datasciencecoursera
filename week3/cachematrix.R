## week 3 peer graded assignment

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set<-function(y)
  {
    x<<-y
    inv<<- NULL
  }
  get <- function() x
  setinv<-function(matrix_inv) inv<<-matrix_inv
  getinv<-function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<- x$getinv()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ##creating the inverse if inv is NULL
  data <- x$get()
  inverse<- solve(data)
  x$setinv(inverse)
  inverse
}
