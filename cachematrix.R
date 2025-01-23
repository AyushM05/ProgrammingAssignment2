## The following functions are designed to cache the inverse of a matrix.
## This can save computation time when the inverse is needed repeatedly,
## especially for large matrices.


## This function creates a special "matrix" object 
## that can cache its inverse. The object includes functions to:
## 1. Set the matrix
## 2. Get the matrix
## 3. Set the inverse of the matrix
## 4. Get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # Initialize the inverse to NULL
    set <- function(y) {
        x <<- y  # Assign the new matrix to x
        inv <<- NULL  # Reset the cached inverse
    }
    get <- function() x  # Retrieve the matrix
    setInverse <- function(inverse) inv <<- inverse  # Cache the inverse
    getInverse <- function() inv  # Retrieve the cached inverse
    list(set = set, get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
}

## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix. If the inverse has already been calculated
## (and the matrix has not changed), it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()  # Check if the inverse is already cached
    if (!is.null(inv)) {
        message("getting cached data")  # Inform the user that cached data is used
        return(inv)  # Return the cached inverse
    }
    data <- x$get()  # Get the matrix
    inv <- solve(data, ...)  # Compute the inverse
    x$setInverse(inv)  # Cache the inverse
    inv  # Return the inverse
}
