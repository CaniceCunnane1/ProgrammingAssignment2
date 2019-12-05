##cacheMatrix.R
## Canice Cunnane Copied the shell of the functions direct from Assigment work 
## 12/05/2019 
## Function makeCacheMatrix and cacheSolve saved in same file

##makeCacheMatrix creates a special "Matrix", which does
##(1) set the value of the matrix
##(2) get the value of the matrix
##(3) set the value of the inverse
##(4) get the value of the inverse



makeCacheMatrix <- function(x = matrix()) {
  ## Return a matrix that is the inverse of 'x'
  
  inver <- NULL
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  get <- function()x
  setinverse <- function(inverse) inver <<- inverse
  getinverse <- function()inver
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## 12/05/2019 
## Canice Cunnane
## Function CacheSolve is a function that computes the inverse of the matrix created
## If the inverse exists, then it will retrieve that while displaying to user that it is 
## retrieving that value from  cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inver <- x$getinverse()
  if(!is.null(inver)) {
    message("getting cached data")
    return(inver)
  }
  matrix <- x$get()
  inver <- solve(matrix, ...) 
  x$setinverse(inver)
  inver
}
