
##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    z <- NULL
    ##set the value of the vector
  set <- function(y) {
          x <<- y
          z <<- NULL
  }
  ##get the value of the vector
  get <- function() x
  ##set the value of the inverse
  setinverse <- function(inverse) z <<- inverse
  ##get the value of the inverse
  getinverse <- function() z
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above
cacheSolve <- function(x, ...) {
  z <- x$getinverse()
  if (!is.null(z)) {
          message("getting cached data")
          return(z)
  }
  ## Return a matrix that is the inverse of 'x'
  data <- x$get()
  z <- solve(data, ...)
  x$setinverse(z)
  z
}

