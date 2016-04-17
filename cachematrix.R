
# makeCacheMatrix performs the followng 4 fnctions
# 1. set the values of a matrix
# 2. get the values of a matrix
# 3. set the inverse values of the matrix
# 4. get the inverse values of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



# The following function returns the inverse of the matrix. 
#Steps :check whether inverse values are existing, if yes , return the cached value and exit from the function 
# If no cache values exists, then perform the inverse
# Please note that non inversable matrix validation is not performed here

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {  # checking for inverse data in the cache
    message("getting cached data.")
    return(inv) # returning the the result with inverse data
  }
  data <- x$get() #continue the function if cached data is not available
  inv <- solve(data)
  x$setinverse(inv)
  return(inv)
}
