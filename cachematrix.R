## makeCacheMatrix creates a special matrix object for caching and cacheSolve checks if inverse is already created for the matrix then return the same else create a inverse using solve().

# makeCacheMatrix: return a list of functions to:
# 1. Set the matrix and its inverse
# 2. Get the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    # inv to store inverse cached matrix
    inv <- NULL

    # Setter for the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    # Getter for the matrix
    get <- function() x

    # Setter for the inverse
    setinv <- function(solve) inv <<- solve
    # Getter for the inverse
    getinv <- function() inv

    # Return the matrix with our newly defined functions
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


# cacheSolve: Compute the inverse of the matrix. If the inverse already
# calculated it returns the cached inverse.

cacheSolve <- function(x, ...) {
  s <- x$getinv()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
##If inverse not yet calculated
  data <- x$get()
  s <- solve(data, ...)
  x$setinv(s)
  s
}
