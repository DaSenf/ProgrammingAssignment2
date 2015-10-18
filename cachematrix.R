# We´d like to create a 'special matrix'. Actually the function returns a list of functions
# that lets us 'set' and 'get' a matrix and also 'set' and 'get' its inverse.
# The cool thing about this is that we only have to compute the inverse of the matrix once and can cache it.
# If we want to use it later on, we just have to call the according 'get'-function.



# You can call the makeCacheMatrix- function without any arguments or with a matrix as an argument.

# Assume that 'data' is a matrix than:   
# 'a <- makeCacheMatrix(data)'
# is equivalent to
# 'a <- makeCasheMatrix()
#  a$set(data)'

makeCacheMatrix <- function(x = matrix()) {
  # For a new 'CacheMatrix' the cache for the inverse is empty:
  i <- NULL
  # you can change the values of the matrix
  set <- function(y) {
    # if you change the values of the matrix, the cache gets emptied
    x <<- y
    i <<- NULL
  }
  # with the get-function you can extract the data you have set before
  get <- function() x
  # with the setInverse-function you create the connection between the matrix and its cached inverse
  setInverse <- function(inverse) i <<- inverse
  # with the getInverse-function you can extract the cached inverse matrix
  getInverse <- function() i
  # the return object is a list of the four functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# The following function calculates the inverse of the 'special matrix' created with the above function.
# However, it first checks to see if the inverse has already been calculated.
# If so, it gets the inverse matrix from the cache and skips the computation.
# Otherwise, it calculates the inverse of the data and sets it in the cache via the setInverse function.

# The function takes a 'special matrix' as an argument, and ... arguments for the solve-function
# Notice that the second argument of the solve-function has to be the identity-matrix with the right dimensions 
# (which is the default) to get the inverse.

cacheSolve <- function(x, ...) {
  # if the inverse is already computed, it doesn´t have to be computed once more
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  # otherwise we get the data
  data <- x$get()
  # and compute the inverse
  i <- solve(data,...)
  # the computed matrix is cached via the setInverse-function
  x$setInverse(i)
  # ... and the inverse is returned
  i
}
