### This is a set of functions that cache the inverse of a matrix

### Create matrix mx

makeCacheMatrix <- function(mx = matrix()) {
      ###Initialize inverse property(matrix) 
  i <- NULL
  
        ###Set matrix
  set <- function( matrix ) {
    mx <<- matrix
    i <<- NULL
}
 ## Method to get the matrix
  get <- function() {
    ## Return the matrix
    mx
  }
  
  ### Method to set the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ### Method to get the inverse of the matrix
  getInverse <- function() {
    ## Return the inverse property
    i
  }
  
  ### Return a list of methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

### Compute the inverse of the special matrix returned by "makeCacheMatrix"
###above. If the inverse already exists(and matrix hasn't
### changed), then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     mx <- x$getInverse()
  
  ###  Return the inverse if already exist
  if( !is.null(mx) ) {
    message("getting cached data")
    return(mx)
  }
  
  ### Get the matrix from our object
  data <- x$get()
  
  ### Calculate the inverse of matrix 
  mx <- solve(data) %*% data
  
  ### Set the inverse to the object
  x$setInverse(mx)
  
  ### Return matrix
  mx
}   
}
