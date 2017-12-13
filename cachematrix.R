##I will write a pair of functions that cache the inverse 
##of a matrix, the matrix supplied is always invertible.


##This function creates a special "matrix" 
##object that can cache its inverse

makecacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinvPerm <- function(invPerm) inv <<- invPerm
  getinvPerm <- function() inv
  list(set = set, get = get,
       setinvPerm = setinvPerm,
       getinvPerm = getinvPerm)
}



##This function computes the inverse of 
##the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
  inv <- x$getinvPerm()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinvPerm(inv)
  inv
}