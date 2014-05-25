## Put comments here that give an overall description of what your
## functions do

## The two functions in this script optimize the calculation of the inverse 
## of a given matrix

## 1) The function makeCacheMatrix creates a data structure to cache the input
## matrix and its inverse, and returns a set of helper functions

## 2) The function cacheSolve retuns the inverse of the matrix cached by 
## makeCacheMatrix, using the helper functions it returns

## makeCacheMatrix
## This function caches a matrix and its inverse (once calculated)
## Input: A valid matrix
## Output: A list of functions (set, get, setinv, getinv)

## Given an input matrix, this function returns a list of 4 functions that
## can be used to
## 1) Retrieve the cached matrix itself
## 2) Retrieve the current value of the inverse 
## 3) Set (or update) the cached matrix
## 4) Set (or update) the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv_m <- NULL
  set <- function(y) {
    x <<- y
    inv_m <<- NULL
  }
  get <- function() {
    x
  }
  setinv <- function(m) {
    inv_m <<- m
  }
  getinv <- function(){
    inv_m
  }
  list(set=set, get=get, 
       setinv = setinv, getinv=getinv)
}


## cacheSolve
## Used in conjunction with the makeCacheMatrix function (above), this function
## allows fast calculation of matrix inverses
## Input -> a cacheMatrix list returned by makeCacheMatrix
## Output -> inverse of the matrix currently stored in the cacheMatrix

## If the inverse for the current matrix has been calculated already, i.e,
## getinv() returns a non-null value, it is returned to the caller
## If the matrix inverse has not been calculated, i.e., getinv() returns a
## NULL value, then the inverse is calculated using solve, and stored in
## cacheMatrix using setinv()
## The inverse is retrieved using getinv() and returned to the caller.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_m <- x$getinv()
  if(!is.null(inv_m)) {
    message("getting cached data")
    return(inv_m)
  }
  x$setinv(solve(x$get()))
  x$getinv()
}
