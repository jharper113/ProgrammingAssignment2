

## These two functions use a cache to store the inverse of a matrix
## and then check the cache before computing the inverse again



# FUNCTION: makeCacheMatrix -----------------------------------------------

## makeCacheMatrix creates a special matrix which is really a list containing:
##   1. set the value of the matrix
##   2. get the value of the matrix
##   3. set the value of the matrix inverse
##   4. get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
    
  # sets the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # gets the value of the matrix
  get <- function() x
 
  # sets the value of the matrix inverse
  setInverse <- function(solve)m <<- solve

  # gets the value of the matrix inverse
  getInverse <- function()m
  
  # returns a list of set/get values
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



# FUNCTION: cacheSolve ----------------------------------------------------

## Computes the inverse of the matrix, but first checks the cache to see
## if the inverse has already been computed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
    m <- x$getInverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
  

# TEST VALUES -------------------------------------------------------------

# testmatrix <- matrix(c(4,2,5,2),2,2)
# solve(testmatrix)
# z <- makeCacheMatrix(testmatrix)
# cacheSolve(z)

