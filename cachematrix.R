#clear our environment variables and console output.
ls()
rm(list=ls())
cat("\014") 

## The makeCacheMatrix function will take an mxn matrix
## where m=n and return a list of functions which can be
## used to cache the result of a matrix inversion.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve will return the cached inverse of a
## matrix if it exists, otherwise it will compute
## the inverse and store it for later use.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    print("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m  
}

# verify that solve will invert a mxn matrix where m=n
# (if it is invertible)
# matrix: a
#     [,1] [,2]
#[1,]    4    3
#[2,]    3    2

a <- matrix(c(4, 3, 3, 2), nrow = 2, ncol = 2)
a
i <- solve(a)
i

# matrix: a-1 or i
#     [,1] [,2]
#[1,]   -2    3
#[2,]    3   -4

# Now lets test that our functions return the same value
# but instead return it from the cache on subsequent calls
# to cacheSolve.

# test that the cache function works, meaning
# it will return the cached inverse matrix of a
x <- makeCacheMatrix(a)

# verify first call to cacheSolve doesn't print 'getting cached data'
# since the inverse of matrix a isn't yet in the cache.
cacheSolve(x)

# this second call will now print 'getting cached data' to the console
# since matrix a is now in the cache.
y <- cacheSolve(x)

# verify that the cached matrix is identical
# to i which we verified previously using the
# solve directly.
identical(x$getinverse(),i)
