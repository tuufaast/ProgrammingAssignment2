## R -programming course, assignment 2

## This function caches the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  #set the matrix to cache
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  #get the matrix
  get <- function() x
  #set the inverse to cache
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  

}


## This funcition returns the inverse of a matrix. If the inverse
## has been solved earlier, then the cached one is returned, otherwise, the
## inverse is computed (and cached)

cacheSolve <- function(x, ...) {
  ##
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  ##if the cached inverse already exists, return it
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  #if not, calculate the inverse
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  m
  
}
