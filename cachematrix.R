## This function is created as a special "matrix" object
## that is capable of caching its inverse

## Caching the Inverse of a Matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(b){
    x <<- b
    i <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
  
}

## This function will calculate the inverse of the special "matrix" created by
## makeCacheMatrix. If the inverse of the matrix "x" has already been calculated 
## and the matrix has not changed, then it retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)){
    message("this is from the cache")
    return(i)
  }
  mat <- x$get()
  i <- solve(mat,...)
  x$setInverse(i)
  i

}
