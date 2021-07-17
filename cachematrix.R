## Caching the Inverse of a Matrix
# R Programming Coursera Week 3 Peer-reviewed assignment

# makeCacheMatrix will set the value of matrix 
# and deletes potentially preceding matrices.
# the get function retrieves the matrix from the set function
# and the setinvert function calculates for the inverse of matrix x
# finally, getinvert function retrieves the value from setinvert

makeCacheMatrix <- function(x = matrix()) {
  v <- NULL
  set <- function(f){
    x <<- f
    v <<- NULL
  }
  get <- function(){x}
  setinvert <- function(solve) v <<- solve
  getinvert <- function(){v}
  list(set = set,
       get = get,
       setinvert = setinvert,
       getinvert=getinvert)
}

# the cachesolve function will first retrieve a possibly
# existing value of an inverse of matrix.
# If there is already the value, it gets the cache and
# return that value. Otherwise, it will assign the matrix to a
# variable "data", solve its inverse and return that value

cacheSolve <- function(x, ...) {
  v<-x$getinvert()
  if(!is.null(v)){
    return(v)
  }
  data <- x$get()
  v <- solve(data,...)
  x$setinvert(v)
  v
}

## Checking my results...
m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
m1
myMatrix_object <- makeCacheMatrix(m1)
cacheSolve(myMatrix_object)
cacheSolve(myMatrix_object)

n2 <- matrix(c(5/8, -1/8, -7/8, 3/8), nrow = 2, ncol = 2)
myMatrix_object$set(n2)
cacheSolve(myMatrix_object)
cacheSolve(myMatrix_object)