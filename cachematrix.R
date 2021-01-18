## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix will contain other functions(set, get, setInverse, and 
## getInverse) to cache the inverse matrix function

makeCacheMatrix <- function(x= matrix()){ ##This function takes the argument matrix
  inv <- NULL  ## This initializes the inverse to null
  setVal <- function(y){ ##This side function sets the value of the matrix
    x <<- y
    inv <<- NULL
  }
  getVal <- function() {x}   ## This function gets the value of matrix x
  setInverse <- function(inverse) {inv <<- inverse} ## This side function sets 
  ##the value of the inverse
  getInverse <- function() {inv}   ## This function gets the inverse values of 
  ##the matrix
  list(setVal = setVal, getVal = getVal, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve calculates the inverse of the cached or "special" matrix

cacheSolve <- function(x, ...)  ## gets cached data
  {
  inv <- x$getInverse() #returns inverse of x and sets to inv
  if(!is.null(inv)){    ##checks whether or not the inverse is null
    message("getting cache data")
    return(inv)   ## returns the inverse
  }
  mat <- x$getVal()
  inv <- solve(mat, ...) ##solves for the inverse values
  x$setInverse(inv)
  inv   ## returns full inverse matrix of matrix x
}

#My Example
m <- makeCacheMatrix(matrix(c(3, 4, 5, 4), nrow = 2, ncol = 2))

m$getInverse()
cacheSolve(m)
m$getInverse()
