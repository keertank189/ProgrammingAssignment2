## This file contains functions, written as part of Coursera's
## R Programming course Programming Assignment 2. 
## These functions were written as an exercise to demonstrate R's
## lexical scoping capabilities. 
## These functions follow the getter and setter style

## This function makes a cache matrix, consisting of getters and
## setters. In effect, then, getting a matrix's inverse
## gets it from the cache rather than recalculating the same. 

makeCacheMatrix <- function(x = matrix()) 
{
  # initializing the inverse matrix to NULL
  inv_mat <- NULL
  
  # getter function for the inverse matrix
  get_mat.invr <- function() 
  {
    inv_mat
  }
  
  # setter function for the inverse matrix
  set_mat.invr <- function(setinv) 
  {
    inv_mat <<- setinv
  }
  
  # getter function for matrix
  get_mat <- function() x
  
  # setter function for matrix
  set_mat <- function(mx) 
  {
    x <<- mx
    inv_mat <<- NULL
  }
  
  # returns a list of getter and setter functions
  list(set = set_mat, get = get_mat,set.invr = set_mat.invr,get.invr = get_mat.invr)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  # CHECK IF CACHE VERSION OF MATRIX EXISTS
  cache_inv <- x$get.invr()
  
  #CASE : CASE VERSION EXISTS
  if(!is.null(cache_inv)) 
  {
    message("Cache Version Found. Returning....")
    return(cache_inv)
  }
  # CASE : CACHE VERSION DOES NOT EXIST
  # solve is called to compute inverse
  orig_mat <- x$get()
  inv <- solve(orig_mat, ...)
  x$set.invr(inv)
  
  # returns the inverse
  inv
}
