# FUNCTION Descriptions
# makeCacheMatrix
#   1). Accepts a matrix as input
#   2). 3 functions are created
#       a). get: grabs matrix from the input
#       b). setinv: sets inverted matrix
#       c). getinv: gets inverted matrix

# cacheSolve
#   1). Grab inverted matrix
#   2). Check to see if the inverted matrix exists.
#       a). If yes, then return it
#       b). If not, then calculate it
#   3). Cache the result
#   4). Get the result and print it

# The function accepts a matrix. Please note that "set" function is not needed as
#   it is not called in cacheSolve.
makeCacheMatrix <- function(mat = matrix()) {
  
  # Assign empty value to inv.
  inv <- NULL
  
  # Anonymous function to get numeric matrix.
  get <- function() mat
  
  # Anonymous funciton to store inverted matrix (caching location).
  setinv <- function(invmat) inv <<- invmat
  
  # Anonymous function to get inverted matrix from cache.
  getinv <- function() inv
  
  # Store get, setinv, and getinv in a list. This will be called from cacheSolve.
  list(get = get,
       setinv = setinv,
       getinv = getinv)
}

cacheSolve <- function(x,...) {
  
  # Get the inverted matrix.
  inv <- x$getinv()
  
  # Check to see if the inverted matrix exists or has changed. If either, then get new one;
  #   otherwise return it.
  if(!is.null(inv)) {
    message("Cached data already exists.")
    return(inv)
  }
  
  # Call the get function and assign to data.
  data <- x$get()
  
  # Calculate the inverse of the matrix and set equal to inv.
  inv <- solve(data)
  
  # Cache the data.
  x$setinv(inv)
  
  # Extract inverted matrix from cache.
  inv
  
}
