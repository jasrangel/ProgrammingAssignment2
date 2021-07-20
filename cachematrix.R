## Week 3 Programming Assignment 2: Catching the Inverse of a Matrix 
#The following functions help to cache the inverse of a matrix 

## makeCacheMatrix: this function creates a matrix that can later be cached for its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL 
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x 
  setinverse <- function(inverse) {m <<- inverse}
  getinverse <- function() m 
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


## cacheSolve: this function will compute the inverse of the above matrix created 

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix,...)
  x$setinverse(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
