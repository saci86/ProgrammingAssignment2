## Week 3, Programming Assignment 2: Lexical Scoping
## This pair of functions cache the inverse of the matrix.
## After creating the matrix object, with makeCacheMatrix(), you can run the
## cacheSolve function to get the inverse. If you run it twice, at the second
## time, it will return the catched data
## Test: 
## > m1 <- matrix(c(4, 3, 8, 2), nrow = 2, ncol = 2)
## > myMatrix_object <- makeCacheMatrix(m1)
## > cacheSolve(myMatrix_object)
## > m2 <- cacheSolve(myMatrix_object)
## here you will get the message of "getting cached data"
## m2 
## [,1]  [,2]
## [1,] -0.1250  0.50
## [2,]  0.1875 -0.25
## > m1 %*% m2
## [,1] [,2]
## [1,]    1    0
## [2,]    0    1


## makeCacheMatrix function creates a special vector, a list with 4 functions 
## to set and get the value of the matrix and the inverse. In case we change the
## value of the matrix, it will automatically reset the inverse to NULL

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve returns the inverse of the matrix returned by makeCacheMatrix. 
## It checks if the inverse is already calculated, and if yes, then return this
## matrix. Otherwise it will use the get data, calculate inverse, set the 
## inverse value sequence to calculate the inverse. 

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}