## Overall description:
## makeCacheMatrix() is defined to save the input and  calculated objects to it's
### enclosing enviromnet, aka 'cached', so it can be retrieved by cacheSolve().
### This avoids having to recalculate the object if it has already been done.
## 
## makeCacheMatrix() calculates the generalized inverse matrix (MASS::ginv) of 
###  a matrix and caches that matrix so that cacheSolve can retrieve the 
###  calculated object if it exisits in the enclosing environment
### the 'superassignment' operator and R's lexical scoping rules
###  are used to cache the calculated matrix
## makeCacheMatrix() takes a single argument, x, 
###  which must be a numeric (or complex) matrix.
## Four functions are defined and returned as a list.
###  set() is defined to assign a new matrix to the enclosing environment
###  get() is defined to return the matrix
###  setginv() is defined to assign the MASS::ginv function available
#### to the enclosing environment and therefore can be used by cacheSolve()
###  getginv() is defined to retrieve the calculated matrix
## MASS::ginv can be substituted for solve()
### to allow non-square matrices and matrices that are
### not the coefficients of a linear system.
# install.packages("MASS")
# library(MASS)

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL   # reset return variable
        set <- function(y) {
                x <<- y        # set x in enclosing.env
                m <<- NULL
        }
        get <- function() x
        #library(MASS)
        #setginv <- function(ginv) m <<- ginv  # set m in enclosing.env
        setginv <- function(solve) m <<- solve # set m in enclosing.env
        getginv <- function() m
        list(set = set, get = get, setginv = setginv, getginv = getginv)
}


## cacheSolve() takes the list 'object' created by makeCacheMatrix as it's arg
## if the inverse matrix was calculated, it retrieve it, if not, it calculates it.
## use MASS::ginv for generalize inverse matrix, 
## or solve() for matrices of coeficients of linear equations

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getginv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        #m <- ginv(data, ...)  ## if using MASS::ginv
        m <- solve(data, ...)  ## for inv of matrix of coef. of linear equations
        x$setginv(m)
        m
}
