## This file creates two connected functions. The first function, makeCacheMatrix creates a special "matrix", 
## object which sets the value of matrix, gets the value of matrix, sets the inverse value of matrix and 
## gets the inverse value of matrix. The second function below calculates the inverse of the special "matrix" 
## created with the makeCacheMatrix. However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the 
## data and sets the value of the inverse in the cache via the setinverse function.


## A function that creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {			# input x will be a matrix
	m <- NULL	 	#  m will be our 'inverse Matrix' and it's reset to NULL every time makeCacheMatrix is called
        
    set <- function(y) {	 # takes an input matrix
			x <<- y			 # saves the input matrix
            m <<- NULL		 # resets the inverse matrix to NULL.
    }
    get <- function() x		 # this function returns the value of the original matrix
    setinverse <- function(inverse) m <<- inverse		# this will store the inverse matrix value using superassignment
    getinverse <- function() m		# this will return the cached value to cacheSolve() on subsequent accesses
    list(set = set, get = get,		# This is a list of the internal functions ('methods')
         setinverse = setinverse,	# so a calling function knows how to access those methods
         getinverse = getinverse)
}


##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##  If the inverse has already been calculated (and the matrix has not changed), then the 
##	cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {	# the input x is an object created by makeCacheMatrix
        
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()		# accesses the object 'x' and gets the value of the inverse matrix
        if(!is.null(m)) {		# if mean was already cached (not NULL)
            message("getting cached data")		# print this this message to the console
            return(m)			# and return the inverse matrix ... ends 
        }
        data <- x$get()			# if x$getinverse() returned NULL get the original matrix
        m <- solve(data, ...)	# compute the inverse matrix
        x$setinverse(m)			# store the computed inverse value in x in makeCacheMatrix
        m			# return the inverse matrix
}
