# R-Programming-Ass-2

makeCacheMatrix <- function(x = matrix()){
  j <- NULL
  set <- function(y){
    x <<- y
    j<<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) j <<- inverse.gaussian
  getInverse <- function() j
  list(set = set, get = get, setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  j <- x$getInverse()
  if(!is.null(j)){
    messsage("getting cache data")
    return(j)
  }
  mat <- x$get()
  j <- solve(mat, ...)
  x$setInverse(j)
  j
}
