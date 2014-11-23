# Matrix inversion is usually a costly computation and there 
# may be some benefit to caching the inverse of a matrix rather 
#than computing it repeatedly

# function returns a list of functions to perform following tasks
# -set the matrix
# -get the martix
# -set the inverse of the matrix
# -get the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
  
  # variable noting whether or not matrix inversion has 
  # already been calculated, default to not calculated
  matrix_inv <- NULL
  
  # assign new matrix to global environment
  # set matrix inversion to not calculated
  set <- function(y) {
    x <<- y
    matrix_inv <<- NULL
  }
  
  # return the matrix
  get <- function() x
  
  # save the inverse of the matrix in global environment
  setinverse <- function(inverse) matrix_inv <<- inverse
  
  # return the inverse matrix
  getinverse <- function() matrix_inv
  
  # return list of functions
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# function returns the inverse of a matrix after checking to see if 
# the inverse was already calculated

cacheSolve <- function(x, ...) {
  
  # retrieve inverted matrix from global environment
  matrix_inv <- x$getinverse()
  
  # if inverted matrix is not null then it has been calculated
  # return the inverted matrix
  if(!is.null(matrix_inv)) {
    message("getting cached data.")
    return(matrix_inv)
  }
  
  # otherwise retrieve the matrix from environment
  # and calculate the inverse
  matrix_inv <- solve(x$get())
  
  # call function to save inverted matrix
  x$setinverse(matrix_inv)
  
  # return inverted matrix
  matrix_inv
}


