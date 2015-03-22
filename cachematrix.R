# makeCacheMatrix
## This function creates a special "matrix" object that can cache its inverse. It 
## returns a list which contains functions to set the matrix, get the matrix, set 
## the inverse of the matrix and get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()){      ### initialize a funciton which requires a 
                                                ### matrix input. 
        
        i<-NULL                                 ### define i and assigns it a NULL value
        
        set <- function(y){                     ### create an object "function()" called
                x <<- y                         ### "set" which replaces an existing
                i <<- NULL                      ### variable "x" for "y" in one of the
        }                                       ### one of the parent environments of the
                                                ### functon.
        
        get <- function() x 
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,              ### return a list with the functions to set 
             setinv = setinv,                   ### the matrix, get the matrix,set the inverse 
             getinv = getinv)                   ### of the matrix and get he inverse of the
                                                ### matrix.
}


# cacheSolve
## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix using the function solve().

cacheSolve <- function(x,...){                  ### initialize a function which requires
        ### a makeCacheMatrix object.
        i <- x$getinv()                         ### subset getinv value from the 
        ### makeCacheMatrix object and assign it to i
        matrix <- x$get()
        i <- solve(matrix, ...)                 ### calculate the inverse matrix with solve()
        ### and assign it to i
        x$setinv(i) 
        i                                       ### Return a matrix i that is the inverse of x
}

# To test the functions:
m <- matrix(c(..),nrows,ncolumns)               ### for example m <- matrix(c(-1, -2, 1, 1), 2,2)
a<-makeCacheMatrix(m)
cacheSolve(a)                                   ### Returns the cached inverse of the matrix
