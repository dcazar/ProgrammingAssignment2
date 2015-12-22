## The functions below can be used to calculate and then store the value of the inverse of a matrix. The inverse of a matrix is 
## a matrix that will provide 1s on the diagonal and 0s for all other values. 
## A variable containing a matrix can be passed to the makeCacheMatrix function. For example:
## M1 <- matrix(c(1, 2, 3, 4), nrow=2, ncol=2)
## cMatrix<-MakeCacheMatrix(M1)
## It is then possible to use the child functions of makeCacheMatrix on the variable containing the matrix. These are:
## cacheMatrix$set(M2)  # Change the matrix being cached.
## cMatrix$get()    # Returns the cached matrix
## cMatrix$getInv() # Returns the inverse of the matrix.


makeCacheMatrix <- function(x = matrix()) {

 m_inv <- NULL
  set <- function(y) {
    x <<- y
    m_inv <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) m_inv <<- inverse
  getInv <- function() m_inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## cacheSolve Will first check to see if the m_inv value is NULL in the makeCacheMatrix environment.
## If it is null, the valaue is calcualted. If it is not, then the value is returned from memory and a 
## message is printed to that fact.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  m_inv <- x$getInv()
  ## Check to see if m_inv has a value, return the cached value if it does, calculate it if not.
  if(!is.null(m_inv)) {
    message("getting cached inverse matrix")
    return(m_inv)
  }
  data <- x$get()
  m_inv <- solve(data,...)
  x$setInv(m_inv)
  m_inv
}

