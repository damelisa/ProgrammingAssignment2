## Function makeCacheMatrix adds a cache to my matrix
## Function cacheSolve searches for the inverse matrix and generates one if necessary

## Summary makeCacheMatrix
# set the inverse matrix to NULL
# generate set function for setting the matrix
# generate get function for getting the matrix
# generate setinverse function for setting the cache of the inverse matrix
# generate getinverse function for getting the cache of the inverse matrix

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


## Summary cacheSolve
# set the inverse matrix to NULL
# generate set function for setting the matrix
# generate get function for getting the matrix
# generate setinverse function for setting the cache of the inverse matrix
# generate getinverse function for getting the cache of the inverse matrix
# check the cache of the inverse matrix to see if an inverse matrix is present
# if a inverse matrix is present, return it
# if no inverse matrix is present, define the data to the matrix original data
# then calculate the inverse matrix of the matrix original data
# set the cache to the calculated inverse matrix
# and return the inverse matrix

cacheSolve <- function(x, ...) {
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
