makeCacheMatrix <- function(x = matrix()) {
  
  ## set and get matrix
  ## get inverse
  ## set inverse requires cachSolve()
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() { x }
  setInverse <- function(solve) { m <<- solve }
  getInverse <- function() { m }
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
  ##this last line asigns a list of 4 functions as 
  ##value returned by the makeCacheMatrix function
  
}

cacheSolve <- function(x, ...) {
  
  m <- x$getInverse()
  
  ## if cacheSolve has already been evaluated
  ## for that matrix, return the existing value
  if(!is.null(m)) {           
    message("getting cached data") 
    return(m)                
  }
  
  ## otherwise, evaluate inverse and cache in $setInverse
  data <- x$get()             
  m <- solve(data, ...)        
  x$setInverse(m)
  
  ## return inverse
  m                           
}