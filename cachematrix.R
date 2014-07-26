## these functions cache the inverse of a matrix

## this function creates a special "matrix" object
## that can cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  ## initialize the i(nverse) to Null during the first
  ## call of the makeCacheMatrix function.
  ## we must first calculate the inverse in cacheSolve().
  i <- NULL
  
  ## function to set a new value for the underlying matrix.
  ## this invalidates the cached inverse, i.
  set <- function(y){
    ## we use the <<- operator to set the value of x and i
    ## because we want to modify x and i defined in the enclosing
    ## environment (created when makeCacheMatrix() was first called),
    ## not in the environment local to set(),
    ## in which x and i are undefined.
    x <<- y

    ## we must reset i to NULL since we are modifying the underlying
    ## matrix and the cached value is no longer the valid.
    i <<- NULL
  }
  
  ## getter function for underlying matrix.
  get <- function() x

  ## set the inverse of the matrix x. Called by cacheSolve.
  setinverse <- function(inverse) i <<- inverse
  
  ## returns the inverse. Will be NULL if setinverse has not been called or
  ## if set is called after the last call to setinverse.
  getinverse <- function() i
  
  ## return value of the makeCacheMatrix function is a list
  ## of functions that we want to expose as public. these are accessed
  ## with the $ operator. Any variables declared inside makeCacheMatrix
  ## but not exported as part of this list are private...they are
  ## inaccessible to any caller of makeCacheMatrix.
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special
## 'matrix' returned by makeCacheMatrix.
## If the inverse has already been calculated
## (and the matrix has not changed), then the 
## cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## get the inverse of the matrix defined inside x.
  ## we can use the $ operator to access the function since
  ## it was defined in the list of function pointers returned
  ## by the call to makeCacheMatrix.
  i <- x$getinverse()

  ## if we've already computed the inverse and stored it via
  ## setinverse(), and have not invalidated the cache by calling
  ## set(), return the cached version of x.
  if(!is.null(i)) {
    message("getting cached data")
    ## we have to explicily use return here otherwise we'd keep
    ## executing the code after the if conditional ends. Since
    ## the cached version is good, just return it and we are done.
    return(i)
  }
  
  ## either we haven't the cached version yet, or we've called 
  ## set() previously and invalidated the cache.
  
  ## call get() to get the underlying matrix
  data <- x$get()
  
  ## calculate the inverse of the underlying matrix, passing with it
  ## any varargs passed to cachesolve
  i <- solve(data, ...)
  
  ## now we set the inverse in x so we cache it and don't need endlessly
  ## recompute it
  x$setinverse(i)

  ## return the caching matrix
  i
}
