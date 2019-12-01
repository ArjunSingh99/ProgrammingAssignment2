## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##This function initializes and holds the values and inverses of matrices
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL #initialize inv as NULL
  set <- function(y) {
    x <<- y #set new value of matrix in parent environment
    inv <<- NULL #reset if matrix is different
  }
  get <- function() x #return the matrix
  setinv <- function(minv) inv <<- minv #assign value of inv in parent environment
  getinv <- function() inv #return value of inv
  list(set = set, get = get, #to subset x with $
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
#this function computes the inverse of a matrix if it doesn't exist, or calls for it if it exists
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv() #to get value of matrix
  if(!is.null(inv)) { #to see if inv is cached or not
    message("getting cached data")
    return(inv)
  }
  data <- x$get() #assigns matrix
  inv <- solve(data, ...) #gets inverse
  x$setinv(inv) #caches the inverse
  inv
}
