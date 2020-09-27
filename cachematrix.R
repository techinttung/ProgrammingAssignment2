##Put comments here that give an overall description of what your
## functions do
##This file include two functions: makeCacheMatrix and cachesolve
## makeCacheMatrix produces an object storing the matrix and its inverse 
## with function to change the matrix and inverse
## the inverse of the matrix is calculated in cachesolve. The function check
## whether the inverse is already stored and print it out if it is
## if not it calculates a new inverse matrix, store it and print it out.

## Write a short comment describing this function
## makecachematrix create a list with 4 items which are theset function 
## to set new matrix in case we want to change the matrix, get function if
## we want to get matrix from the one that has already stored, setinv for
## setting new inv matrix incase we don't have it and getinv is for getting 
## the cache inv.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
## cache solve is where the calculation happen, first it checks if there 
## there is already a solved inverse in the object we look at if there is
## then it returns the cached values but if not the function calculate new 
## inv matrix and and store it back into the object 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
