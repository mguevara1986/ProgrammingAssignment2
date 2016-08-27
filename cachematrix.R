## Put comments here that give an overall description of what your
## functions do

## This function creates a list containing a function that
#1. set the value of the matrix
#2. get the value of the matrix
#3. set the value of the inverse of the matrix
#4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  # set the value of the matrix
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  #get the value of the matrix
  get <- function() x
  #set the value of the inverse of the matrix
  setinverse <- function(inverse) m <<-inverse
  #get the value of the inverse of the matrix
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned
#by makeCacheMatrix above. If the inverse has already been calculated
#(and the matrix has not changed), then the cachesolve should retrieve 
#the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  #Check to see if the inverse has already been calculated. 
  #If so, it gets the inverse from the cache and skips the computation
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  #If not, get the matrix and compute its inverse 
  #(assume that matrix will always be invertible)
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
