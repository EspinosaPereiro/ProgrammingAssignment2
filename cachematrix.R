##Caching the inverse of a matrix
#These two functions interact to provide the inverse of a given matrix. 
#If the inverse has been calculated before, the second function can recover it from the cache.

#1. makeCacheMatrix: This function prints a list of four elements that subsequently set and get the
#matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setmatrix <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getmatrix <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinv = setinv,
       getinv = getinv)
}

#2. cacheSolve: This function computes the inverse of the matrix returned by makeCacheMatrix above using the solve() function. 
#When the inverse is not null (i.e., it was calculated before), the function recovers the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  data <- x$getmatrix()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}


##Testing the two functions above
#Example matrix
n <- rnorm(16, 4, 1) #The solve() function will produce an error if the matrix determinant is 0.
x <- matrix(n, 4, 4)

#Applying the two functions subsequently, the cachesolve computes the inverse of the matrix
x1 <- makeCacheMatrix(x)
cacheSolve(x1)

#After computing the matrix inverse, the second application of the function provides the inverse from cache
cacheSolve(x1)