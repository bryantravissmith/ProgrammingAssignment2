## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

require(MASS)  #include mass to call ginv function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(m){
    x <<- m
    m <<- NULL
  }
  get <- function() x
  getinverse <- function() inv
  setinverse <- function(ginv) inv <<- ginv
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- ginv(data,...)
  x$setinverse(inv)
  inv
}
