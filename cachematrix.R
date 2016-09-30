makeCacheMatrix <- function(x = matrix()) {
  ## x: a matrix that you can invert
  ## returns: a list with the below functions
  ##         1. set the matrix
  ##         2. get the matrix
  ##         3. set the inverse
  ##         4. get the inverse
  ##         this list then is used in cacheSolve()
  
          inv = NULL
          set = function(y) {
            ## <<- this assigns a value to an object for the environment 
            ## different from the current environment
                  x <<- y
                  inv <<- NULL
          }
  
    get = function() x
    setinv = function(inverse) inv <<- inverse 
    getinv = function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
  
  ## x is the output from makeCacheMatrix()
  ## returns: the inverse of the original matrix put into makeCacheMatrix()
  inv = x$getinv()
  
      # if the inverse has already been calculated
        if (!is.null(inv)){
        # get it from the cache and skips the computation. 
        message("producing cache data")
        return(inv)
  }
  
  ## alternatively calculate the inverse 
      mat.data = x$get()
  
        inv = solve(mat.data, ...)
  
        ## sets the value of the inverse in the cache via the setinv function.
        x$setinv(inv)
        return(inv)
}
