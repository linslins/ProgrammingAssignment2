## Put comments here that give an overall description of what your
## functions do:
      ## Function makeCacheMatrix serves the purpose of storing and setting 
      ## values of the matrix (particularly the inverse) to be used in further
      ## calculations,while cacheSolve only calculates the inverse, 
      ## and resets the value in makeCacheMatrix.
      ## This way, if the matrix is big, we don´t need to run the inverse
      ## everytime we call it.

## Write a short comment describing this function
      ## makeCacheMatrix stores the value of the matrix and its inverse, 
      ## and also gives the option to reset the values (both for the matrix 
      ## definition and its inverse).
      ## We use '<<-' because we want to define the values not only within the 
      ## function'set', but all over the makeCacheMatrix.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinv <- function(inverse) inv <<- inverse
      getinv <- function() inv
      list(set=set, get=get, setinv=setinv, getinv=getinv)  
}


## Write a short comment describing this function
      ## cacheSolve is a function that first checks if there is a cached value
      ## of the inverse of the matrix (and if this is the case, it retrieves
      ## such value to avoid performing the calculation). And if there is no
      ## stored value, it calculates it with the 'solve' function and then 
      ## sets this value with makeCacheMatrix so it is available to be used
      ## in the future (using the form 'x$setinv').
      ## This function is not considering checking if the matrix is invertible
      ## (like, the matrix being squared, determinant != 0,...), it assumes
      ## it is.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getinv()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      inv
}
