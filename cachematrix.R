## Put comments here that give an overall description of what your
## functions do

##sets the value of the matrix
##gets the value of the matrix
##sets the value of the inverse
##gets the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL     
  set<- function(y) { 
    x<<-y
    inv<<-NULL
  }
  get<- function()x 
  setInverse <- function(inverse) inv <<- inverse
  getInverse<- function() inv 
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Write a short comment describing this function
##checks if the inverse of a matrix has been computed 
##if not, it will compute the inverse of a matrix
##if so, it will retrieve the already computed inverse from the cache

cacheSolve <- function(x, ...) {
  inv<- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    retunr(inv)
  }
  mat<- x$get()
  inv<- solve(mat, ...)
  x$setInverse(inv)
  inv
}