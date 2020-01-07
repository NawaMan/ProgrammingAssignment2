
## The following function returns a matrix that cache its inverse 
##   so that the inverse can be accessed multiple time after.
## The retult metrix has the following methods:
##   - set : to set the value of the metrix
##   - get : to get the metrix
##   - setinverse : to change the inverse metrix of this metrix
##   - getinverse : to get the inverse metrix of this metrix
makeCacheMatrix <- function(the_metrix = matrix()) {
  invert_metrix <- NULL
  set <- function(new_value) {
    the_metrix <<- new_value
    invert_metrix <<- NULL
  }
  get <- function() the_metrix
  setinverse <- function(inverse) invert_metrix <<- inverse
  getinverse <- function() invert_metrix
  list(
    set=set, 
    get=get, 
    setinverse=setinverse, 
    getinverse=getinverse
  )
}


## The following function returns the inverse of the matrix that are cached.
## Usage:
##     my_metrix = makeCacheMatrix()
##     inverse_my_metrix = cacheSolve(my_metrix)
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