## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) s <<- solve
  getinverse <- function() s
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$get()
  s <- x$getinverse()
  if(!is.null(s)) {
    message("getting cached matrix")
    return(s)
  } else if(det(m) == 0){
    warning("matrix not invertible")
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinverse(s)
  s
}
