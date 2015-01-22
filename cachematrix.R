## Put comments here that give an overall description of what your
## 1. The functions below make a special matrix that caches its inverse value and 
## 2. It  uses the cached value if available rather than to compute it again which could be a costly operation
## 3. We make use of the <<- operator to cache the inverse in a different environment than where it is defined
## functions do

## Write a short comment describing this function
## makecacheMatrix creates a special matrix that caches its inverse value
## It returns a list of functions to access and manipulate the underlying matrix and the inverse value

makeCacheMatrix <- function(x = matrix()) {
  matrix_inverse <- NULL;

  ## Copies the matrix argument y to be cached into the matrix_m
  set <- function(y)
  {
    matrix_m <<- y
	matrix_inverse <<- NULL
  }
  
  ## Returns the underlying matrix
  get <- function() matrix_m
  
  ##Returns the cached inverse if any else NULL value shall be returned
  getinverse <- function() matrix_inverse

  ## Sets the cached inverse value
  setinverse <- function(inverse)
  {
     matrix_inverse <<- inverse
  }

  list(set = set, get = get, setinverse = setinverse,getinverse = getinverse)

}


## Write a short comment describing this function
## 1. The function cacheSolve takes the special matrix created above by makeCacheMatrix
## 2. It checks if the inverse is already computed . If so it uses the cached value .Else it shall compute and then cache the inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inverse <- x$getinverse()
		if(!is.null(inverse))
		{
		   message("getting cached inverse")
		   return (inverse) 
		}

		mat <- x$get()
		inverse <- solve(mat)
		x$setinverse(inverse)
		inverse
}
