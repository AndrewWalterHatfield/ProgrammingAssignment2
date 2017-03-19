## Two functions that cache the inverse of a matrix

## This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ivrs <- NULL
  set <- function(y){
    x <<- y
    ivrs <<- NULL
  }
  get <- function() x
  setinverse <- function(solvematrix) ivrs <<- solvematrix
  getinverse <- function() ivrs
  list(set = set, get = get, setinverse = setinverse, 
       getinverse = getinverse)
}


## This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the 
## inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ivrs <- x$getinverse()
  if(!is.null(ivrs)){
    message("getting cached data")
    return(ivrs)
  }
  data <- x$get()
  ivrs <- solve(data)
  x$setinverse(ivrs)
  ivrs    
}
