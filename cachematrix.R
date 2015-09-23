##
# cachematrix.R
#
# Contains two methods used to create, compute and cache the inverse 
# of a matrix. CacheMatrix improves performance calculating the inverse
# once, and returning the cached inverted matrix for subsequent calls.

##
# Create a CacheMatrix which is a special matrix that is capable of 
# storing/caching its inverse.
# 
# Note: This function assumes all matrix passed are square invertable.
#
# Args:
#	x = A aquare invertable matrix.
# 
# Returns:
#	A special matrix with 
#
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL

    set <- function(mat) {
        x <<- mat
        inv <<- NULL
    }

    get <- function() { x }

    setinv <- function(inverse) { inv <<- inverse } 

    getinv <- function() { inv }

    list(set = set,
         get = get,
         setinv = setinv,
         getinv = getinv)
}

##
# Computes the inverse of a square matrix or retreives it 
# from the cacheMatrix's internal cache.
#
# Args:
#	x = CacheMatrix created using the makeCacheMatrix factory method
#
# Returns
# 	The 'inverse matrix' of the CacheMatrix
#
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) { 
        message("getting cached data.")
        return(inv) 
    }

    x$setinv(solve(x$get()))
    x$getinv()
}
