## makeVector will be used as a template
## makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse 
## 4. get the value of inverse 

makeCacheMatrix <- function(x = matrix()) {
  ## numeric replaced with matrix as we go to two dimensions
  ## other local variables are retained
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) m <<- inverse
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

# cacheSolve will check if the inverse has already been computed. If so, skips the
# computation and returns the inverse. Otherwise it computes the inverse

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    ## inverse already computed, return it
    message("getting cached data")
    return(m)
  }
  ## compute the inverse 
  data <- x$get()
  ## solve will invert the matrix
  m <- solve(data)
  x$setinv(m)
  m
}
