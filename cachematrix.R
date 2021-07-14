## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function( x = numeric()) {
  m <- NULL   #creating memory space for m
  set <- function(y) {
    x <<- y   #assign the input argument to x/m in the parent 
    m <<- NULL  #environment #clears m from previous caches
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve  
  #assign the input argument to the value of m in the parent environment
  getInverse <- function() m
  list( set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse) 
  #assigns each of these functions as an named element within a list()
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  m <- x$getInverse() #load the input object
  if(!is.null(m)) {
    message("getting cached data")
    return(m) #checks to see whether the result is NULL if not -> return m+massage
  }
  data <- x$get() #if the matric has not been inverted use solve to invert
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
