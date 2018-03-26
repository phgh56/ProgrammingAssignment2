## Creating function with list of functions to be used later when 
## creating the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
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


## Function that creates the inverse of a matrix or retrieves the inverse of a matrix
## (if it wasn't in the cache) and prints the output

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    dat <- x$get()
    m <- solve(dat)
    x$setinv(m)
    m
  }
