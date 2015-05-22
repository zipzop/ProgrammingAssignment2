## The pair of functions below will cache the inverse of a matrix


## This function creates a special "matrix" object that can cache its inverse
## x stores the matrix, i stores the inverse
## get will get the value of the matrix 
## set will set the value of the matrix
## setinverse stores the inverse of the matrix 
## getinverse returns the inverse of the matrix or Null if not previously set by setinverse.
## Note, everytime set is called the inverse (i) is set to null ensuring any change to the matrix ,
## means a new inverse must be calculated. 

makeCacheMatrix <- function(x = matrix()) {
  
  x <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function computes the inverse using the solve function of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
    i <- x$getinverse()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
  
  
}
