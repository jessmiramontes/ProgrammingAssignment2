## Functions that cache the inverse of a matrix


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  xinverted <- NULL # variable to store inverted matrix
  # A setter function to set a matrix to object created by makeCacheMatrix function
  # makeCacheMatrix(tstmatrx) # to work on tstmatrx
  # makeCacheMatrix$set(tstmatrx1) # to work on tstmatrx1
  set <- function(y) {
    x <<- y
    xinverted <<- NULL # initialises xinverted to null
  }
  
  get <- function() x # return the input matrix
  setInv <- function(inv) xinverted <<- inv # set the inversed matrix
  getInv <- function() xinverted # return the inversed matrix
  # return a list that contains these functions, so that we can use the
  # makeCacheMatrix object
  # x <- makeCacheMatrix(testmatrix)
  # x$set(newmatrix) # to change matrix
  # x$get # to get the setted matrix
  # x$setInv # to set the inversed matrix
  # x$getInv # to get the inversed matrix
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
    

}


## This function computes the inverse of the special "matrix" 
##  returned by makeCacheMatrix above. If the inverse has already 
##  been calculated (and the matrix has not changed), 
##  then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInv() # get the inversed matrix from object x
  # it will be null if uncalculated
  if(!is.null(m)) { # if there is an inversion result
    message("getting cached data")
    return(m) # return the calculated inversion
  }
  data <- x$get() # if not, we do x$get to get the matrix object
  m <- solve(data) # solve it
  x$setInv(m) # set it to the object
  m # return the solved result



}

