#Following functions will compute the inverse of a square matrix and
# cache it. If the attempt is made to inverse the matrix which is in cache 
# the the function simply return the cached result rather then re-calculation
# the inverse.

#usage:
#  > a <-  makeCacheMatrix()
#  > a$setMatrix(matrix(rnorm(4),2,2))
#  > cacheSolve(a)
#  [,1]      [,2]
#  [1,] -0.8651774 0.1743651
#  [2,]  1.0181888 0.4664599
#
#  > cacheSolve(a)
#  using cached matrix
#  [,1]      [,2]
#  [1,] -0.8651774 0.1743651
#  [2,]  1.0181888 0.4664599


#This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x=matrix()) {
  m <- NULL
  i_m <- NULL
  setMatrix <- function (y) {
    m <<- y
    i_m <<- NULL
  }
  getMatrix <- function () m
  getInverseMatrix <- function () i_m
  
  setInverseMatrix <- function (z) i_m <<- z
  list ( setMatrix= setMatrix, 
         getMatrix=getMatrix, 
         getInverseMatrix=getInverseMatrix,
         setInverseMatrix=setInverseMatrix )	
}

#This function computes the inverse of the special "matrix" returned by
# makeCacheMatrix. If the inverse has already been calculated (and the matrix 
# has not changed), then the 'cachesolve' should retrieve the inverse from the
# cache.
cacheSolve <- function(x, ...){
  i_m <- x$getInverseMatrix()
  # NOTE: a square matrix is not invertible if det(matrix) == 0
  isIdentityMatrix <- function (i , i_i) {
    tempM <- i %*% i_i == diag(nrow = nrow(i), ncol = ncol(i))
    !("FALSE" %in% (levels(factor(tempM))))
  } 
  i_m <- x$getInverseMatrix()
  if(!is.null(i_m)) {
    message("using cached matrix")
    return(i_m)    
  }
  #check if 'x' is a square invertible matrix
  m <- x$getMatrix()
  i_m <- solve(m, ...)
  if (det(m) == 0) {
    message("warning: matrix is not invertible as det(m) is 0.")
  }
  x$setInverseMatrix( i_m )
  i_m
}
