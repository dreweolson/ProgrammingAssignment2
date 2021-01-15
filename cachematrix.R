## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix will contain other functions(set, get, setInverse, and 
## getInverse) to cache the inverse matrix function

makeCacheMatrix <- function(x= matrix()){
  inv <- NULL  ## This initializes the inverse to null
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}   ## This function gets matrix x
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}   ## This function produces inverse of matrix
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve obtains cached data

cacheSolve <- function(x, ...)  ## gets cached data
  {
  inv <- x$getInverse()
  if(!is.null(inv)){    ##checks whether or not the inverse is null
    message("getting cache data")
    return(inv)   ## returns the inverse
  }
  mat <- x$get()
  inv <- solve(mat, ...) ##solves for the inverse values
  x$setInverse(inv)
  inv   ## returns full inverse matrix of matrix x
}

#My Example
m <- makeCacheMatrix(matrix(c(3, 4, 5, 4), nrow = 2, ncol = 2))

m$getInverse()
cacheSolve(m)
m$getInverse()
