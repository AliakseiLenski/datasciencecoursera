setwd("C:/Users/Aliaksei/Desktop/datasciencecoursera/ProgrammingAssignment2-master")
## Creating two functions to work with matrices and their inverses
## Matrix inversion is usually a costly computation and there 
##may be some benefit to caching the inverse of a matrix 
##rather than compute it repeatedly

##This function creates a matrix object that can cashe its inverse
## for the input

makeCacheMatrix <- function(x = matrix())
  {
 inv <- NULL
 set <- function(y)
 {
   x <<- y
   inv <<- NULL
 }
 get <- function()
{
   x
 }
 setInverse <- function(inverse)
 {
   inv <<- inverse
 }
   
 getInverse <- function()
 {
   inv
 }
 
 list(set = set,
      get=get,
      setInverse = setInverse,
      getInverse = getInverse)
}


## cacheSolve is a function which computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cacheSolve should retrieve the 
## inverse from the cache

cacheSolve <- function(x, ...) 
  {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv))
  {
    return (inv)
  }
  m <- x$get()
  inv <-solve(m, ...)
  x$setInverse(inv)
  inv
}
