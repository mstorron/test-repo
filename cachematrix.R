## makeCacheMatrix stores in cache the inverse of a matrix


makeCacheMatrix  <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse  <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve computes the inverse of the matrix returned by makeCacheMatrix
## or retrieves it from cache if it's already been calculated

cacheSolve <- function(x, ...) {  ## Return a matrix that is the inverse of 'x'
  
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting inverse data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}


