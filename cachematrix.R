#A pair of functions that first makes a cacheable square matrix and then solves the matrix for its inverse but only after checking 
#if the very same matrix has been solved for before using this function, thus has a cached solved inverse in memory.

## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
## It is a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {				#input x is defined as matrix

		i <- NULL										#i will hold value for inverse matrix. Reset to NULL each time makeCacheMatrix is called.
														#i value set within bounds of makeCacheMatrix
														
		#Following three functions are defined but not run when makeCacheMatrix() is called.
        #Instead, they will be used by cacheSolve() to get values for x or for
        #i (inverse) and for setting the inverse. These are usually called object 'methods'

	    set <- function(y) {
	        x <<- y										#value of x in global env assigned NULL
	        i <<- NULL									#value of i in global env assigned NULL
	        											#when set fct is called, it will use the global values of x and i
	    }
	    get <- function() x								#fct doesn't require argument; fetches matrix x
	    setinverse <- function(inverse) i <<- inverse	#this is called by cacheSolve() during the first cacheSolve() access
	    												#it will store the value using superassignment 
	    getinverse <- function() i						#will return the cache value of to cacheSolve on subsequent accesses
	    
	    list(											#List of internal functions is accessed each time makeCacheMatrix is called,  
	        set = set,	    							#i.e each time we make a new object. 
	        get = get,	    							#This list helps calling functions know how to access these methods.
	        setinverse = setinverse,
	        getinverse = getinverse)

}


## The cacheSolve function calculates the inverse of the special "matrix"
## which was created with the makeCacheMatrix function.
## However, it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse
## in the cache via the setinverse function.


cacheSolve <- function(x, ...) {				#input x is object created by makeCacheMatrix
    
    ## Return a matrix that is the inverse of 'x'
    
    i <- x$getinverse()							#access value of i, & gets value of inverse of matrix
    if(!is.null(i)) {							#check if i is not null, i.e. if inverse has been calculated and stored previously
        message("getting cached data")			#send this message to console
        return(i)								#return cached value of i. Ends function cacheSolve().
    }
    data <- x$get()								#else, if i is NULL, it gets matrix and stores in variable 'data'; passed by reference
    i <- solve(data, ...)						#solve function calculated inverse of the given matrix
    x$setinverse(i)								#set inverse value in i. In the future, i will be not null for this matrix.
    i											#return inverse value i
}
