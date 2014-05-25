## This function creates a special "matrix" object that can cache its inverse.
## x is any square invertible matrix

## The `solve` function is used to compute the inverse  
#  for a  square invertible matrix `X`, 
# `solve(X)` returns its inverse.

makeCacheMatrix <- function(x = matrix()) {
      xinv <- NULL
      set <- function(y) {
            x <<- y
            xinv <<- NULL
      }
      get <- function() x
      setinv <- function(solve) xinv <<- solve
      getinv <- function() xinv
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
# above. If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      xinv <- x$getinv()
      if(!is.null(xinv)) {
            message("getting cached data")
            return(xinv)
      }
      data <- x$get()
      xinv <- solve(data, ...)
      x$setinv(xinv)
      xinv
}





####################################################################################
# The following code was used to test the functions.  The test cases are taken from
# a discussion group thread.  I have modified the test cases.
# Test case code from:  https://class.coursera.org/rprog-003/forum/thread?thread_id=650
# Author: Gregory D. Horne.  I also modified the code since we used different names in functions
# (e.g. "getinverse" changed to "getinv", since we used different names
####################################################################################

#Use simple test case of identity matrix with 5's on the diagonal.
# the inverse will also be an identity matrix with 1/5=0.20 on the diagonal

amatrix = makeCacheMatrix(matrix(c(5,0,0,5), nrow=2, ncol=2))
amatrix$get()         # Returns original matrix
cacheSolve(amatrix)   # Computes, caches, and returns    matrix inverse
amatrix$getinv()  # Returns matrix inverse
cacheSolve(amatrix)   # Returns cached matrix inverse using previously computed matrix inverse

# test 2:  another diagonal, inverse should be diagonal with 1/4=0.25, and 1/2=0.5 on diagonal
amatrix$set(matrix(c(4,0,0,2), nrow=2, ncol=2)) # Modify existing matrix
cacheSolve(amatrix)   # Computes, caches, and returns new matrix inverse
amatrix$get()         # Returns matrix
amatrix$getinv()  # Returns matrix inverse

# test 3:  10x10 matrix of random normals with mean 5 and sd=2
set.seed(281)
# create 10x10 matrix of random normals with mean=7 and sd=2
amatrix$set(matrix(rnorm(100, mean=5, sd=2 ), nrow=5, ncol=5)) 
cacheSolve(amatrix)   # Computes, caches, and returns new matrix inverse
amatrix$get()         # Returns matrix
amatrix$getinv()  # Returns matrix inverse

