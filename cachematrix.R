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
