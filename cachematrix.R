## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function will create an object that caches its inverse
# makeCacheMatrix is made up of set, get, setInverse and getInverse

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL  #to create the inverse to be NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x} #to get matrix x
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv} #to get inverse of matrix x
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Write a short comment describing this function
# This function will compute the inverse of the same matrix
# If it is already computed, it will bring the inverse from the cache

cacheSolve <- function(x, ...){  #to retrieve cache data
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){  #to check if inverse is NULL
    message("getting cached data")
    return(inv) #to return the inverse values
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
