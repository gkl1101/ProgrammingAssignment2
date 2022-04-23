## Put comments here that give an overall description of what your
## functions do

## As an example I used this matrix:
##     |1 -1|
##     |2  3|

## set - set the matrix
## get - get the matrix
## set_inverse_mat - it sets the cached inverse matrix
## get_inverse_mat - it gets the cached inverse matrix

## Write a short comment describing this function
## This function creates a special "matrix" 
## object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inverse_mat <- NULL
        
        set <- function(y){
                matrix <<- y
                inverse_mat <<- NULL
        }
        
        get <- function() {
                matrix
        }
        
        set_inverse_mat <- function(inverse){
                inverse_mat <<- inverse
        }
        
        get_inverse_mat <- function(){
                inverse_mat
        }
        
        list(set = set, get = get, set_inverse_mat = set_inverse_mat, get_inverse_mat = get_inverse_mat )
}

        
## Write a short comment describing this function
## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse_mat <- x$get_inverse()
        if (!is.null(inverse_mat)){
                return(inverse_mat)
        }
        
        m <- x$get()
        
        inverse_mat <- solve(m, ...)
        x$set_inverse_mat(inverse_mat)
        inverse_mat
}

-----------------------------
x <- makeCacheMatrix(matrix(c(1,2,-1,3),ncol = 2,nrow = 2))
cacheSolve(x)


