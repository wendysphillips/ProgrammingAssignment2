## These two functions together determine the inverse of a matrix and once having calculated it store it in cached memory.

## makeCacheMatrix creates an R object that stores a matrix (x) and stores its inverse (mat) after the first calculation of that inverse, and it builds a set of functions (set, get, setinverse, getinverse) that get stored within a list in the parent environment to be used by the cacheSolve function. It first creates an empty matrix so that cacheSolve can check for an empty vs. already calculated matrix.

makeCacheMatrix <- function(x = matrix()) {
      mat <- matrix(data=NA, nrow=1, ncol=1)
      set <- function(y) {
            x <<- y
            mat <<- matrix(data = NA, nrow=1, ncol=1)
      }
      get <- function() x
      setinverse <- function(inv) mat <<- inv
      getinverse <- function() mat
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)      
}

##cacheSolve first checks to see if the inverse of the matrix has already been calculated. If it was already calculated, it reports the stored data with the message "getting cached data." If the inverse has not yet been calcuated, cacheSolve calculates the inverse and then stores it in the variable mat located in the parent environment.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      ## Note, I changed the function(x,...) to function(z,...) because I am less confused that way.
      mat <- z$getinverse()
      if(!is.na(mat[1,1])) {
            message("getting cached data")
            return(mat)
      }
      data <- z$get()
      mat <- solve(data, ...)
      z$setinverse(mat)
      mat
}