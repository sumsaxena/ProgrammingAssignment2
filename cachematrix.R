## Caching the inverse of a matrix (assuming matrix is invertible)


## makeCacheMatrix function makes a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## sets inverse to null
  n<- NULL 
  
  ## set function sets matrix as the  argument and resets inverse to null, 
  ## thereby clearing the cache when a new matrix is set
  set <- function(y) {
    x <<- y
    n <- NULL
    
  }
  ## get function returns the matrix
  get <- function() x
  
  ## setinverse sets the value of inverse in n
  setinverse <- function(inverse) n <<- inverse
  
  ## getinverse returns the value of inverse matrix
  getinverse <- function() n
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## cacheSolve functionThis function computes the inverse of the 
## special "matrix" returned by makeCacheMatrix above. If the 
## inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x=matrix(), ...) {
  ## Return a matrix that is the inverse of 'x'
  n <- x$getinverse()
  
  ## checks if the value is already cached and returns it if it does
  if(!is.null(n)) {
    message("getting cached data")
    return(n)
  }
  
  ## inverts the matrix
  data <- x$get()
  n <- solve(data, ...)
  x$setinverse(n)
  n
  
  
}
