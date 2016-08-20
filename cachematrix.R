#This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  #function to set the value of the matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  #function to get the value
  get <- function() x
  
  #function to set the value of the inverse
  setinverse <- function(inverse) i <<- inverse
  
  #function to get the value of the inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    #if I have value <> NULL return value
    message("getting cached data")
    return(i)
  }
  #else, get the data and calculate the inverse
  data <- x$get()
  i <- solve(data, ...)
  #set the value in the list
  x$setinverse(i)
  i
}
