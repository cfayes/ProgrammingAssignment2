## Put comments here that give an overall description of what your
## functions do

## create "Matrix"  object

makeCacheMatrix <- function(x = matrix()){
  invr <- NULL
  
  set <- function(y){
    x <<- y 
    invr <<- NULL}
  
  get <- function() x 
  
  setinverse <- function(solve) invr <<- solve
  
  getinverse <- function() invr
  
  list(set = set, get = get, setinverse = setinverse, getinverse=getinverse)
  
}


## calculate inverse of "Matrix" object or retrieve existing inverse

cacheSolve <- function(x, ...){
  
  invr <- x$getinverse()
  
  if(!is.null(invr)){
    message("getting cached data")
    return(invr)
  }
  
  data <- x$get()
  invr <- solve(data, ...)
  x$setinverse(invr)
  invr
}
