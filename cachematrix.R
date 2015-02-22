## These functions will take a matrix, cache its inverse and return the cached inverse if it is available.
## If the inverse is not available it will calculate the inverse and then cache it.

## This function take a Matrix and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  i = NULL  ## resets i to NULL if it already contained a value
  set <- function(y) {  ## Caches the matrix, resets i within the function "set"
    x <<- y
    i <<- NULL
  }
  get <- function() x  ## returns the original matrix
  setinverse <- function(solve) i <<- solve  ##caches the inverse of the matrix x
  getinverse <- function() i ## Retrieves the cached inverse of the matrix x
  list(set = set, get = get, ## List containing the functions within this object
       setinverse = setinverse,
       getinverse = getinverse)
}




## This function returns a cached version of the matrix x if available. If it is not available it 
## calculates the inverse and then caches it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse() ## Retrieve the cached inverse of matrix x (if it has been calculated)
  if(!is.null(i)) {  ## Checks to see if the there is a cached inverse of matrix x
    message("getting cached data") 
    return(i) ## Returns the cached matrix of x
  }
  data <- x$get() ## If there isn't already a cached version of matrix x, it obtains the original matrix
  i <- solve(data, ...)  ## Calculates the inverse of matrix x
  x$setinverse(i) ## caches the calculated inverse of matrix x
  i
  
  
  
}
