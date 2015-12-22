## This R file can be used to cache the inverse of a matrix, for using in long loops
## It contains two functions:
##    - makeCacheMatrix - To build a matrixCache that is used to store the original and inverted matrix
##    - cacheSole - To solve the inverse of the matrix, either by using the cache or calculating it
##
## Additionally, I've included a test class for easier testing - testCache

## makeCacheMatrix - Stores the original and inverted matrix (if already claculated). It also contains methods
## to get and set both of them
## - x$set - Stores the original matrix
## - x$get -  Retruns the original matrix
## - x$setinverse - Stores the inverse matrix in the cache
## - x$getinverse - Gets the inverse matrix from the cache, if already calculated
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # Stores the original matrix, and resets the calcaulted value
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Gets the original matrix
  get <- function() x
  
  ## Gets the inverse
  setinverse <- function(inverse) inv <<- inverse
  
  ## Stores the inverse
  getinverse <- function() inv
  
  ## Returns a list containing the four methods
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve - Calculates the inverse of a matrix
## The parameter x should be a list created with "makeCacheMatrix" to work.
cacheSolve <- function(x, ...) {
  
  # Try to get the inverse from the cache
  inverse <- x$getinverse()
  
  # If found, just return it
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  # If not found, calculate and return it
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}

## testCache - It can be executed to test if the above functions work or not
testCache <- function() {
  m1 <- rbind(c(1, 2, 3), c(0, 1, 4), c (5, 6,0))
  m2 <- rbind(c(1, 2, 3), c(2, 1, 4), c (5, 8,0))
  
  # Function to check the values
  checkInverse <- function(m, inv)
  {
    if (isTRUE(all.equal(solve(m), inv)))
    {
      message('Test OK')
    }
    else
    {
      message('Test KO')
    }
  }
  
  ## Create the matrix cache
  cachedMatrix <- makeCacheMatrix()
  
  ## Assign the first matrix
  cachedMatrix$set(m1)
  
  ## Calculate inverse -- It should be calculated
  inv <- cacheSolve(cachedMatrix)
  checkInverse(m1, inv)
  
  ## Calculate inverse again -- It should take the cache
  inv <- cacheSolve(cachedMatrix)
  checkInverse(m1, inv)
  
  
  
  ## Stores a new matrix
  cachedMatrix$set(m2)
  
  ## Calculate the inverse -- new matrix - it should be calculated
  inv <- cacheSolve(cachedMatrix)
  checkInverse(m2, inv)
  
  ## Calculate inverse again -- It should take the cache
  inv <- cacheSolve(cachedMatrix)
  checkInverse(m2, inv)
}
