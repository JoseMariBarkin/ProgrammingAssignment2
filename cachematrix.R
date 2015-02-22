## Put comments here that give an overall description of what your
## functions do

## The following makeCacheMatrix function creates a special "matrix",
## which is really a list containing a function to
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse
## - get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    # initialization to NULL
    inv <- NULL
    
    # to set the value of the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL # matrix changed
    }
    # to get the value of the matrix
    get <- function() x
    # to set the inverse
    setinv <- function(inv_) inv <<- inv_
    # to get the inverse
    getinv <- function() inv
    
    # return a list of all the above functions
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function
## The following function calculates the inverse of the special
## "matrix" created with the above function.
## It first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the
## computation. Otherwise, it calculates the inverse of the
## matrix and sets the value of the inverse in the cache via
## the setinv function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    # check if the inverse matrix is already cached
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    # not cached, so we get the matrix into data
    data <- x$get()
    # compute the inverse
    inv <- solve(data, ...)
    # cache the inverse
    x$setinv(inv)
    # return it as well
    inv
}



###############################################
#################### TESTS #################### 
###############################################

test = function(mat){
    temp = makeCacheMatrix(mat)
    
    start.time = Sys.time()
    cacheSolve(temp)
    dur = Sys.time() - start.time
    print(dur)
    
    start.time = Sys.time()
    cacheSolve(temp)
    dur = Sys.time() - start.time
    print(dur)
}

r = rnorm(1000000)
mat1 = matrix(r, nrow=1000, ncol=1000)
test(mat1)

# r = rnorm(100000000)
# mat2 = matrix(r, nrow=10000, ncol=10000)
# test(mat2)


## a <- cbind(c(1,2),c(3,4))
## cacheSolve(makeCacheMatrix(a))




# Test
source("cachematrix.R")
#
# generate matrix, and the inverse of the matrix.
# size of the matrix edge
size <- 1000 
mymatrix <- matrix(rnorm(size^2), nrow=size, ncol=size)
mymatrix.inverse <- solve(mymatrix)
#
# now solve the matrix via the cache-method
#
special.matrix   <- makeCacheMatrix(mymatrix)
#
# this should take long, since it's the first go
special.solved.1 <- cacheSolve(special.matrix)
#
# this should be lightning fast
special.solved.2 <- cacheSolve(special.matrix)
#
# check if all solved matrices are identical
identical(mymatrix.inverse, special.solved.1) & identical(mymatrix.inverse, special.solved.2)
#
# should return TRUE


