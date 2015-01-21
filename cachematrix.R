##
## Brian Dreibelbis
## 1/16/15
##

## Creates cache matrix to pass to cacheSolve() function 
## which sets and gets the inverse matrix.

makeCacheMatrix <- function(originalMatrix = matrix()) {
  
  #Initialize the invertedMatrix to Null. 
  invertedMatrix <- NULL
  
  set <- function(y) {
    originalMatrix <<- y
    invertedMatrix <<- NULL
  }
  
  # Functions for getting and setting inverted matrix value
  get <- function() originalMatrix

  # Inversing the matrix using solve() function
  setInverse <- function(solve) invertedMatrix <<- solve
  getInverse <- function() invertedMatrix
  
  list(
    set = set, 
    get = get,
    setInverse = setInverse,
    getInverse = getInverse)  
}


## Computes the inverse of the cacheable matrix returned by makeCacheMatrix()
## If the inverse has already been calculated and there's no change in the matrix
## then the cacheSolve() returns the cached inverse

cacheSolve <- function(cacheMatrix, ...) {
  invertedMatrix <- cacheMatrix$getInverse()

  # Already inverted matrix?
  if(!is.null(invertedMatrix)) {
    message("Matrix already inverted.")
    return(invertedMatrix)
  }

  # If got here, matrix not inverted.
  # Create inverted matrix.
  matrix <- cacheMatrix$get()
  invertedMatrix <- solve(matrix)
  cacheMatrix$setInverse(invertedMatrix)
  invertedMatrix 
}
