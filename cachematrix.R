##makeCacheMatrix function creates a new matrix
##The first function, makeVector creates a special "vector", which is really a list containing a function to

##set the value of the matrix
##get the value of the matrix
##set the value of the inverse
##get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  ##this is requires for the following cachesolve to work
  
}



cacheSolve <- function(x, ...) {
  ##Added some new comments
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
    ##if already calculated prints the message"getting cached data"
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
  ## Return a matrix that is the inverse of 'x'
}
