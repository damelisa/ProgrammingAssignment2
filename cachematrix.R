## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# make output which contains matrix & inverse of matrix
# 1. set value of matrix
# 2. get the value of the matrix
# 3. set the value of the inverse matrix
# 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  c <- NULL
  set <- function(y) {
    x <<- y
    c <<- NULL
  }
  get <- function () x
  setinverse <- function(solve) c <<- solve
  getinverse <- function() c
  list (set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)

}


## Write a short comment describing this function
# 1. check if cache of inverse matrix is full
# 2. when full, get the inverse of the matrix
# 3. if not full, get the matrix original data
# 4. calculate the inverse of the matrix original data
# 5. return the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  c <- x$getinverse()
  if(!is.null(c)) {
    message ("getting cached data")
    return(c)
  }
  data <- x$get()
  c <- solve(data, ...)
  x$setinverse(c)
  c
}
