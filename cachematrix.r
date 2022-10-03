##Use the "inv" function as an alternative to the "solve" function
##to find the inverse of a matrix

install.packages("matlib")
library("matlib")

#First make an inverse matrix, each item in list can be called individually
#external to the function ie: get(), set()...
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function(){
    inver<-ginv(x)
    inver%*%x #function to obtain inverse of matrix
  }
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This will return the inverse of the matrix if the matrix is not "null"

cacheinverse <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  thing <- x$get()
  inv <- solve(thing, ...)
  x$setinverse(inv)
  inv
}

#Test the functions out below
example<-makeCacheMatrix(matrix(4:10,4,4)) #function 1
example$get()
example$getinv()
cacheinverse(example) #function 2
