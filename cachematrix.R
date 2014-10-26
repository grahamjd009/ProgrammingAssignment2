# Authored: Oct 25, 2014
# By : JDG
# For: Coursera DataSci - R Programming Asignment 2

# "cachematrix.R" takes a matrix as input and calculates, 
# caches and returns the inverse of that matrix
# -----------

# makeCacheMatrix creates a new environment that takes an input matrix 'x',
# creates a variable 'm' and creates a list of functions that allow access and
# modification to x and m. 'x' is the input matrix, 'm' is where we store the 
# cache of its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL       # initial declaration of 'm', the inverse cache
  
  set <- function(y) { 	# function to modify the current matrix
    x <<- y			    # assigns newly input y to outside variable 'x'
    m <<- NULL			# resets inverse cache 'm' to NULL
  }                  	
  
  
  get <- function() { 	# function to return the current matrix stored in 'x'
    x
  }
  setinverse <- function(inverse) { #  function to update our inverse cache 'm' 
    m <<- inverse
  }
  getinverse <- function() {  
    m
  }
  
  list(set = set, get = get,  	 	# allows us to call the individual functions...
       setinverse = setinverse, 	# ... of makeCacheMatrix
       getinverse = getinverse)
}

## cacheSolve computes, caches and returns the inverse of the input matrix

cacheSolve <- function(x, ...) {
  m <- x$getinverse()             	# assigns value of our global inverse cache ... 
                                    # ...'m' to local variable 'm'
  if(!is.null(m)) {
    message("getting cached data")
  }                                
  
  data <- x$get()                   # If the cache is null, use 'get' from ...
                                    # ... makeCacheMatrix to assign input 'x' to 'data'
  m <- solve(data, ...)             # use solve to calculate the inverse and assign to 'm'
  x$setinverse(m)                   # use "setinverse' to update cache
  m	              					
}
