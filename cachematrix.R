################################################
#
# File: cachematrix.R
# Description: Compute the inverse of a matrix and cache that
# Date: 18/6/2014
# Author: CaoXu (rockbean3000@gmail.com)
# Test sample: 
# >sample<-makeCacheMatrix()
# >sample$set(matrix(c(1,0,2,4),2,2))
# >sample$get()
# >cacheSolve(sample)
# >sample$getCache()
################################################

# makeCacheMatrix - Creates a special "matrix" object that can cache its inverse 
# @x: The data matrix
# Returns: A list contains functions: set, get, setCache, getCache
# 1) set, set up the data matrix and cache matrix
# 2) get, get the data matrix
# 3) setCache, set the matrix into cache
# 4) getCache, get the matrix from cache

makeCacheMatrix <- function(x = matrix()) {
  # cache initialize
  cache <- NULL
  # set, data and cache matrix initialization
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  # get, only return data matrix
  get <- function() x
  # setCache, set the matrix into cache
  setCache <- function(matrix) cache <<- matrix
  # getCache, only return cache
  getCache <- function() cache
  # return the list filled with the functions
  list(set = set, get = get,
       setCache = setCache,
       getCache = getCache)
}

# cacheSolve - computes the inverse of the special "matrix" returned by makeCacheMatrix above
# @x: The object created by makeCacheMatrix
# Returns: Either the cached matrix or the inverse of the data matrix if there isn't a cached one
# Caution: The data matrix must be a square invertible one

cacheSolve <- function(x, ...) {
  ## If there isn't a cached matrix, it will compute the inverse of 
  ## the matrix and set the result to the cache
  if(is.null(x$getCache())) {
    x$setCache(solve(x$get()))
  }
  message("getting inversed matrix")
  x$getCache()
}
