#-------------------------------------------------------------------------------
# File          : cachematrix.R
# Version       : 0.9
# Date          : 06 July 2017
# Developer     : Joost van Boxmeer
#-------------------------------------------------------------------------------
# Function      : makeCacheMatrix
# Description   : This function creates a special "matrix" object
#                 that can cache its inverse.
#-------------------------------------------------------------------------------
# Function      : cacheSolve
# Description   : This function computes the inverse of the special "matrix"
#                 returned by function: makeCacheMatrix.
#                 If the inverse has already been calculated
#                 (and the matrix has not changed),
#                 then the cachesolve should retrieve the inverse from the cache.
#-------------------------------------------------------------------------------

# Creates a special "matrix" object
makeCacheMatrix <- function(x = matrix()) {

    mat_inv <- NULL
    # Set the value of the Matrix
    set <- function(y){
            x <<- y
            inv <<- NULL
    }
    # Get the value of the Matrix
    get <- function() x

    # Set the inverse of the Matrix
    set_mat_inv <- function(solve) mat_inv <<- solve

    # Get the inverse of the Matrix
    get_mat_inv <- function() mat_inv
    list(set = set, get = get,
         set_mat_inv = set_mat_inv,
         get_mat_inv = get_mat_inv
         )
}

# Computes the inverse of the special "matrix" created with function makeCacheMatrix.

cacheSolve <- function(x, ...) {

  mat_inv <- x$get_mat_inv()
  if(!is.null(mat_inv)) {
    message("getting cached data")
    return(mat_inv)
  }
  mat <- x$get()
  mat_inv <- solve(mat, ...)
  x$set_mat_inv(mat_inv)
  mat_inv
}
