## This function creates a matrix object, which can be inversed.
## If the inverse of the matrix has been calcaulated, the cacheSolve function retrieves the matrix from the cache


## This function makes a special matrix object
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinverse<-function(inverse) m <<- inverse
  getinverse<- function() m
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
  
}


## This function computes the inverse of a matrix returned by the function makeCacheMatrix.
## If the inverse has been calculated already and the matrix has not changed, this function
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data.")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
