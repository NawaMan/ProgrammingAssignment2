makeCacheMatrix <- function(the_metrix = matrix()) {
  inv <- NULL
  set <- function(new_value) {
    the_metrix <<- new_value
    invert_metrix <<- NULL
  }
  get <- function() the_metrix
  setinverse <- function(inverse) invert_metrix <<- inverse
  getinverse <- function() invert_metrix
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The following function returns the inverse of the matrix

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}