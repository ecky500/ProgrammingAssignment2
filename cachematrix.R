## These two functions create a special matrix that caches a matrix and its inverse
## The get makeCacheMatrix function creates the special matrix and caches its value, and its inverse once created
## The cacheSolve function either solves the matrix's inverse or retrieves it from the special matrix cache.

## This fucntion creates a special matrix with two objects, mat and x, and four functions.
## Functions setMat/getMat are used to cache and retrieve the matrix, respectively
## Functions setInv/getInv are used to cache and retrieve the inverse of the matrix, respectively

makeCacheMatrix <- function(x = matrix()) {
      invMat <- NULL # this initializes the inverse matrix
      
      #sets the special matrix and cache's it at variable y 
      setMat <- function(y) {
            x <<- y
            invMat <<- NULL #if a new matrix is set, invMat is reset
      }
      
      #returns the special matrix
      getMat <- function() x
      
      #sets the matrix inverse
      setInv <-  function(inv) invMat <<- inv
      
      #gets the matrix inverse
      getInv <-  function() invMat
      
      #returns list of matrix and its inverse
      list(setMat = setMat, getMat = getMat, setInv = setInv, getInv = getInv)
}


## This function takes a special matrix list object created by "makeCacheMatrix" and gets or creates and returns cached inverse

cacheSolve <- function(x, ...) {
       ## Return a matrix that is the inverse of 'x'
      invMat <- x$getInv()
      if(!is.null(invMat)) {
            message("getting cached inverse")
            return(invMat)
      }
      
      mat <- x$getMat()
      invMat <- solve(mat, ...)
      x$setInv(invMat)
      invMat
}
