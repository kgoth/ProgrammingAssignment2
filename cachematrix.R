## this fuction creates a special object called 
## makeCacheMatrix that stores a matrix in the
## environment: 0x0000000013d524d

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
## The below are calls that can be used to extract the
## values of cached objects 
  get <- function()x
  setinv <- function(solve) m <<-solve  #solve function creates
  getinv <- function()m                 #the inverse of a matrix
  list(set = set, get = get, setinv = setinv, 
       getinv = getinv)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## pulling in the inverse of x if it has been calculated
  m <- x$getinv()
  ## if the inverse value is not null, then it will 
  ## print a message saying "getting cached data"
  ## and it will return the cached inverse matrix
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ##bringing the cached matrix x and assigning it to the
  ## object called data
  data <- x$get()
  ## the solve function produces the inverse of the data.
  m <- solve(data, ...)
  ##
  x$setinv(m)
  m
}
