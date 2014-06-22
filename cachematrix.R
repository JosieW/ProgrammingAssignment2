## These functions implement a solution to allow the caching of 
## matrix inversions. These such operations are expensive/timeconsuming
## in terms of processing so if the result is going to be used multiple times
## it makes sense to calculate it once and then reuse the result.

## makeCacheMatrix creates a special object to provide an interface to the data and
## hold the cached result
## cacheSolve works on the object created in the first funtion
## to return a matrix that is the inverse of 'x', either calculating it or using the cached value

 
# The first function makeCacheMatrix creates a special "matrix"
# which is an interface to access the underlying matrix and it's inverse
# It is really just a list containing 4 functions to
# 
# 1.  set the value of the matrix
# 2.  get the value of the matrix
# 3.  set the value of the inverse of the matrix
# 4.  get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# The following function calculates the inverse of the special "matrix"
# created with the above function. However, it first checks to see if the
# inverse has already been calculated. 
# If so, it 'get's the inverse from the cache and skips the computation. 
# Otherwise, it calculates the mean of the data and sets the value 
# in the cache via the 'setinverse' function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
