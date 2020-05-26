## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  # this j is used to store cached value
  # to begin with we initialize it to null
  j <- NULL
  
  #this is used to create a working enviornment
  
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  
  #this is used to get the value of the matrix
  
  get <- function()x
  
  #we invert the matrix and store it in cache denoted by j
  
  setInverse <- function(inverse) j <<- inverse
  
  # we get the inverted matrix from the cache
  getInverse <- function() j 
  
  # we now return the created functions to the working enviornment
  
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  #this attempts to get the inverse of the matrix stored in cache
  
  j <- x$getInverse()
  
  # this is used to return a inverted matrix from cache if it exists 
  #else create the matrix in the working enviornment
  
  if(!is.null(j)){
    message("getting cached data")
    return(j)
  }
  
  #this creates a matrix since it does not exists
  mat <- x$get()
  
  
  j <- solve(mat,...)
  
  #this is used to set inverted matrix in the cache
  x$setInverse(j)
  
  #this is to display matrix in console
  j
}
