## Find the mean of given objects set as matrix in a loop and sets it in sort of a recall mode (cache)
## If, the mean is not available; it process it so that it use it as cache

## find inverse mean in cache; if null, gets inverse mean

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL #assigning mean; if mean is available loop; if not, get mean.
  set <- function(y) { #set the vector
    x <<- y
    inv <<- NULL
  }
  get <- function() x #get vector. Objects "x, m" outside function... why?
  setinverse <- function(inverse) inv <<- inverse #store inverse
  getinverse <- function() inv #if not found, get inverse
  list(set = set, get = get, #store as lists
       setinverse = setinverse,
       getinverse = getinverse)
}


## Retrieves the stored result from the previous function

cacheSolve <- function(x, ...) {
  inv <- x$getinverse() #get cached inverse
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get() #if not inverse, get matrix
  inv <- solve(data, ...) #find the inverse
  x$setinverse(inv) #store as object
  inv
  ## Return a matrix that is the inverse of 'x'
}
