## The goal of the two following functions is to caching 
## the inverse of a matrix rather than compute it repeatedly.

## The makeCacheMatrix function returns a list containing 
## a following function:
##
## 1) setMatr - sets the value of the matrix;
## 2) getMatr - gets the value of the matrix;
## 3) setInverse - sets the value of the inverted matrix;
## 4) getInverse - gets the value of the inverted matrix.

makeCacheMatrix <- function(Matr = matrix()) {
    invMatr <- NULL
  
    setMatr <- function(y) {
        Matr <<- y
        invMatr <<- NULL
    }
  
    getMatr <- function() Matr
    setInverse <- function(Invertse) invMatr <<- Invertse
    getInverse <- function() invMatr
  
    list(   setMatr = setMatr,
            getMatr = getMatr,
         setInverse = setInverse,
         getInverse = getInverse)
}


## The cacheSolve function returns a matrix that is inverse
## of other matrix.
## Firstly cacheSolve checks for inverse of a matrix. If it
## has already been calculated this function returns it by
## retrieving from the cache. Otherwise it computes the 
## inverse and returns computed matrix.

cacheSolve <- function(List, ...) {
    invMatr <- List$getInverse()
  
    if(!is.null(invMatr)) {
        message("Cashed matrix is got")
    
        return(invMatr)
    }
  
    forSolve <- List$getMatr()
    invMatr <- solve(forSolve, ...)
    List$setInverse(invMatr)
    invMatr
}
