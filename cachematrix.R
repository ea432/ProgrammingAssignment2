## The pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      #set the value of the matrix
      calculation <- NULL
      set <- function(y) {
            x <<- y
            calculation <<- NULL
      }
      #get the value of the matrix
      get <- function() x
      #set the value of the inverse
      setinverse <- function(inverse) calculation<<- solve(x)
      #get the value of the inverse
      getinverse <- function() calculation 
      
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      #Checks if the inverse has already been calculated
      #If so, it gets the inverse from the cache and skips the computation
      calculation <- x$getinverse()
      if (!is.null(calculation)) {
            message("getting cached data")
            return(calculation)
      }
      #Otherwise, calcualte the inverse if the matrix
      #and set the value to "setinvers"
      data <- x$get()
      calculation <- solve(data, ...)
      x$setinverse(calculation)
      calculation
      
}
