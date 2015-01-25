## Siddhant Mohalanobish, for R Programming (Coursera)

## This has 2 functions that help us 
## (1) invert matrices
## (2) store the result of the inverted matrices in a cache

## makeCacheMatrix is a helper function that allows us to do (2) above.
## It has 4 functions that lets us 
## set(x)          : Store the matrix value
## get()           : Get the matrix value
## setInverse(inv) : Store the matrix inverse value
## getInverse()    : Get the matrix inverse value
makeCacheMatrix <- function(x = matrix()) {
  # store the value of the inverse internally here
  inverse <- NULL
  
  # set the matrix value, also reset inverse to NULL, in the event that we're reusing the variable for another value
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  # return matrix value
  get <- function() x
  # assign the inverse value
  setInverse <- function(inv) inverse <<- inv
  # return the inverse value
  getInverse <- function() inverse
  # set the functions associated with the CacheMatrix
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function returns the inverse of 'x'.
## If the value exists already, then it returns the cached inverse value
## If not, it computes the inverse and stores the value into cache (and returns the inverse)
cacheSolve <- function(x, ...) {
  # Check if cache has inverse value
  inverse <- x$getInverse()
  # If inverse value exists, return cached data
  if (!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  # Get matrix data otherwise
  data <- x$get()
  # Actually invert the data
  inverse <- solve(data, ...)
  # Save inverse into cache
  x$setInverse(inverse)
  # Return inverse value
  inverse
}

# # Testing this:
# > X = rbind(c(1, 2, 3), c(0, 1, 4), c(5, 6, 0))
# # Creates a 3x3 matrix
# > cacheMatrix = makeCacheMatrix(X)
# # Creates the cacheMatrix
# > cachesolve(m)
# # Actually calculates the inverse. (And returns the value)
# > cachesolve(m)
# # You will notice that it returns the cached result this time. (There will be a "getting cached"                               data message)
