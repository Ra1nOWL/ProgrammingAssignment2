## Assignment: Cache the inverse of a matrix
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  ## Initialization
  inv <- NULL
  
  ## Set matrix
  set <- function(matrix){
    x <<- matrix
    inv <<- NULL
  }
  
  ## Get matrix
  get <- function(){
    x
  }
  
  ## Set inverse matrix
  setInverse <- function(inverse){
    inv <<- inverse
  }
  
  ## Get inverse matrix
  getInverse <- function(){
    inv
  }
  
  ## Return methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  
  ## Return inverse matrix
  mtx <- x$getInverse()
  
  ## If the inverse matrix exists, return it.
  if(!is.null(mtx)){
    message("getting cached data")
    return(mtx)
  }
  
  ## Get matrix
  data <- x$get()
  
  ## Calculating the inverse matrix
  mtx <- solve(data) %*% data
  
  ## Setting inverse matrix
  x$setInverse(mtx)
  
  ## REturn matrix
  mtx
}
