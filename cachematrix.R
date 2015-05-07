## Brian Dister - R Programming Class - Assignment 2
## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly.
## This code includes a pair of functions that creates a special 
## "matrix" object that can cache its inverse.


## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  
  if(!is.null(inv)) {
    message("getting cached inverted matrix")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv  
}

# Use this function by passing an invertible matrix
# to exercise the matrix functions above
test = function(mat){
  
  matA = makeCacheMatrix(mat)
  
  start = Sys.time()
  cacheSolve(matA)  # need to calculate the inverse and store it the first time
  duration = Sys.time() - start
  print(duration)
  
  start = Sys.time()
  cacheSolve(matA)  # should pull from cache this time
  duration = Sys.time() - start
  print(duration)
}

# test using test function above.
# set.seed(23002)
# r = rnorm(1000000)
# matA = matrix(r, nrow=1000, ncol=1000)
# test(matA)
