## makeCacheMatrix calculates the inverse of a user-defined matrix and caches the result.
## cacheSolve then calls on the cache if the matrix has not changed, otherwise it calculates the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {       ## Caches the inverse of pre-existing user-defined matrix x.
  m <- matrix(NA)
  set <- function(y) {
    x <<- y
    m <<- matrix(NA)
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {                  ## Calls on the cache if the matrix has not changed, otherwise 
  m <- x$getinverse()                             ## calculates the inverse of the matrix.
  if(!is.na(m[1,1])) {                
    message("getting cached data")                ## Return a matrix that is the inverse of 'x'
    return(m)
  }
  data <- x$get()                                 ## If the inverse of x has not been calculated and cached previously,
  m <- solve(data)                                ## then it is calculated, cached, and returned here.
  x$setinverse(m)
  m
}
