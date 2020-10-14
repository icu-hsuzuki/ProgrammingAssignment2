## Assginment of a coursera course R Programming in Week 3
## 
## This is to create two functions as follows
## 1. makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.
## 2. cacheSolve: This function computes the inverse of 
## the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated 
## (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## 
## e.g. z <- makeCacheMatrix(matrix(c(1,0,1,1), nrow = 2, byrow = FALSE))
### Note that byrow = FALSE is default
## cacheSolve(z)
##      [,1] [,2]
## [1,]    1   -1
## [2,]    0    1

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}