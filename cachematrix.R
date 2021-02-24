#Assignment 2 - week 3
## Put comments here that give an overall description of what your 
## functions do 

## Write a short comment describing this function 

# The following function creates a special matrix that can cache it's inverse.

makeCacheMatrix <- function(x = matrix()) { 
  inv <- NULL 
  set <- function(y) { 
    x <<- y 
    inv <<- NULL 
  } 
  get <- function() x 
  setInverse <- function(inverse) inv <<- inverse 
  getInverse <- function() inv 
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) 
}

## Write a short comment describing this function 
# The following function calculates the inverse of the "matrix" created with
# the  function makeCacheMatrix
# It proofs to see if the inverse has already been calculated.
# If so, it returns the inverse from the cache and everything is ok.
# otherwise, it calculates the inverse of the data and sets the value of the
# inverse in the cache via the setInverse function

cacheSolve <- function(x, ...) { 
  ## Return a matrix that is the inverse of 'x' 
  inv <- x$getInverse() 
  if (!is.null(inv)) { 
    message("getting cached data") 
    return(inv) 
  } 
  data <- x$get() 
  inv <- solve(data, ...) 
  x$setInverse(inv) 
  inv 
} 

#testing 
testmatrix <- makeCacheMatrix(matrix(1:4,2,2))
testmatrix$getInverse()
cacheSolve(testmatrix)
testmatrix$getInverse()
testmatrix$set(matrix(c(3,3,1,4),2,2))
testmatrix$get()
testmatrix$getInverse()
cacheSolve(testmatrix)
testmatrix$getInverse()
