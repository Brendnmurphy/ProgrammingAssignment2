## makeCacheMatrix creates a list with functions that can be called when 
## calculating the inverse of a matrix. This list object will store the value of 
## the inverse matrix, so when cacheSolve is called a second time on the same 
## matrix, the inverse can be returned without needing to calculate it twice.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL 
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x #Gets the matrix passed to the function
  setinverse <- function(inverse) i <<- inverse #Allows the inverse to be updated
  getinverse <- function() i #Retrieves the currently stored inverse
  ## The functions get stored in a list so they can be easily called by another
  ## function, in this case cacheSolve
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve takes the list  created by the makeCacheMatrix function and first 
## checks if there is a stored value for the inverse matrix. If there is, it
## return a message and then the inverse matrix. If there is no stored inverse 
## matrix, one will be calculated then the list will be updated to contain the 
## inverse matrix, before finally it is returned.

cacheSolve <- function(x, ...) {
  i <- x$getinverse() ##Assigns existing value of the inverse
  if (!is.null(i)) { ## If there is an existing (non-null) value for i, use that
    message("getting cached data")
    return(i)
  }
  data <- x$get() #Get the matrix you want to  invert 
  i <- solve(data, ...) #Invert it and store in i
  x$setinverse(i) #Update the list with the inverse of the matrix
  i #return the inverse of the matrix
  }
