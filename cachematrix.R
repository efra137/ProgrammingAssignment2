## Put comments here that give an overall description of what your
## functions do

## This function creates a matrix that can be cached, it asumes matrix is inversable

makeCacheMatrix <- function(x = matrix()) {
  iv  <- NULL
  set  <- function(y){
    x <<- y
    iv <<- NULL 
  }
  get  <- function() x
  setinverse  <- function(inverse) iv  <<- inverse
  getinverse  <- function() iv
  list(set= set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
  
}


## Write a short comment describing this function

## This function uses solve() to inverse the matrix. It asumes matrix is inversable
## It first verify if it is already cached by testing !is.null

cacheSolve <- function(x, ...) {
  iv  <- x$getinverse()
  if (!is.null(iv)){
   message("Working hard to get the data cached :-)")
   return(iv)
  }
  data  <- x$get()
  iv  <- solve(data, ...)
  x$setinverse(iv)
  iv
}




