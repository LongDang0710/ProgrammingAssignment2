## This pair of functions caches the inverse of a matrix to avoid repeated 
## and potentially costly computation. The makeCacheMatrix function creates 
## a special matrix object that can store the original matrix and its inverse.
## The cacheSolve function computes the inverse of the special matrix object 
## if it is not already cached.

## This function creates a special "matrix" object that can cache its inverse.
## It includes methods to set and get the matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the inverse property
  
  # Method to set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset the inverse cache when the matrix is reset
  }
  
  # Method to get the matrix
  get <- function() x
  
  # Method to set the inverse of the matrix
  setinverse <- function(inverse) inv <<- inverse
  
  # Method to get the inverse of the matrix
  getinverse <- function() inv
  
  # Return a list of the methods
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix. If the inverse has already been calculated (and the 
## matrix has not changed), it retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  # If the inverse is already cached, return it
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Otherwise, calculate the inverse
  data <- x$get()
  inv <- solve(data, ...)  # Compute the inverse
  
  # Cache the calculated inverse
  x$setinverse(inv)
  
  # Return the inverse
  inv
}
