## the first function creates a matrix that can be cached
## the second function returns an inverse of the supplied matrix

# this function creates a matrix object, where the inverse can be cached into memory

makeCacheMatrix <- function(x = matrix()) {
          inverse_matrix <- NULL
          set <- function(y) {
            x <<- y
            inverse_matrix <- NULL
          }
          get <- function() x
          set_inverse <- function(inverse) inverse_matrix <<- inverse
          get_inverse <- function() inverse_matrix
          list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## This function first searches for the inverse of the function in cache, if not found, only then is the inverse calculated

cacheSolve <- function(x, ...) {
          inverse_matrix <- x$get_inverse()
          if(!is.null(inverse_matrix)){
            message("getting cached data")
            return(inverse_matrix)
          }
          matrix <- x$get()
          inverse_matrix <- solve(matrix, ...)
          x$set_inverse(inverse_matrix)
          inverse_matrix
}
