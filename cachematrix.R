## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function caches the inverse of the matrix x

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y){
    x <<- y
    im <<- NULL
  }
  
  get <- function() x
  setInverse <- function(solve) im <<- solve
  getInverse <- function() im
  
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)

}




## cacheSolve calculates the inverse of matrix x. If the reverse of matrix is already cached, the cached value is returned else it is computed again.

cacheSolve <- function(x, ...) {
  im <- x$getInverse()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  data <- x$get()
  im <- solve(data, ...)
  x$setInverse(im)
  im
        ## Return a matrix that is the inverse of 'x'
}



