## Lexical Scoping assignment for R Programming Coursera Course

## makeCacheMatrix, provides a list of functions getter and setters in order 
## to provide a cached "class" of it inverse. Inverse matrix is not auto-calculated
## so it requires from cacheSolve function.

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


## cacheSolve requires a makeCacheMatrix function, in order to get the inverse of
## a matrix provided through 'solve' R function. In conjunction these two functions 
## make sure inverse matrix is only calculated once.

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
