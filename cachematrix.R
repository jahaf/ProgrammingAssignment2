## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
 m <- NULL## initial seed
  set <- function(y) {
    x <<- y  ## cache x by y (the input to the func)
    m <<- NULL ## introduce cache m
  }
  
  ##set the value of the matrix
  ##get the value of the matrix
  ##set the value of the inverse
  ##get the value of the inverse
  
  
  get <- function() x 
  setinv <- function(Inv) m <<- Inv 
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## x is the makeCacheMatrix function to initialize the cache
  ## First get the Inverse 
  m <- x$getinv()
  
  ## If it is not NULL, it means it has been calculated before
  ## we can retrieve it from the cache, so we can RETURN
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## Otherwise, we need to calculate the inverse using SOLVE
  ## We will set it to the cache to store it for future use
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
}
