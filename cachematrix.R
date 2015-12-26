##############################################################
#               Johns Hopkins University                     #
#                   R Programming                            #
#       Programming Assignment 2: Lexical Scoping            #
#                                                  		     #
#                                                            #
# 	Author:          Mohamed TOUITI                          #
##############################################################

##############################################################
# Summary : Matrix inversion is usually a costly computation #
#  and there may be some benefit to caching the inverse      #
#  of a matrix rather than compute it repeatedly.            #
#  The following two functions are used to cache the inverse # 
#  of a matrix.                                              #
##############################################################


## The function "makeCacheMatrix" creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        ## @x: a square invertible matrix
        ## return: a list containing functions to
        ##              1. set the matrix
        ##              2. get the matrix
        ##              3. set the inverse
        ##              4. get the inverse
    
    # matrixInverse will store the cached inverse matrix
    matrixInverse <- NULL
    
    # Setter for the matrix
    set <- function(y) {
        x <<- y
        matrixInverse <<- NULL
    }
    # Getter for the matrix
    get <- function() x
    
    # Setter for the inverse
    setInverse <- function(inverse) matrixInverse <<- inverse
    
    # Getter for the inverse
    getInverse <- function() matrixInverse
    
    # Return the matrix with our newly defined functions
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The "cacheSolve" function computes the inverse of the special matrix returned by the function "makeCacheMatrix".
cacheSolve <- function(x, ...) {
        ## @x: output of makeCacheMatrix()
        ## return: inverse of the original matrix input to makeCacheMatrix()
    
    # get the inverse of the matrix.
    matrixInverse <- x$getInverse()
    
    # If the inverse has already been calculated then it retrieves the inverse from the cache.
    if(!is.null(matrixInverse)) {
        message("Getting cached data")
        return(matrixInverse)
    }
    
    # If the inverse doesnt exist the function calculates the inverse and save it.
    data <- x$get()
    matrixInverse <- solve(data, ...)
    
    # Cache the inverse
    x$setInverse(matrixInverse)
    
    # Return the inverse
    matrixInverse
}
