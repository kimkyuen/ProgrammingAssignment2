## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) { #define function argument with x as a matrix by default
  inv <- NULL #initialize object inv as NULL, will hold inverse matrix
  set <- function(y) { #define set function to take in argument y
    x <<- y #assign x in parent environment
    inv <<- NULL #assign inv in parent environment to NULL. will clear any value of inv that was cached by a previous execution of cachesolve()
    
  }
  get <- function() x #defines the getter for matrix x
  setinverse <- function(inverse) inv <<- inverse #assigns the value of inv in parent environment
  getinverse <- function() inv #gets the value of inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) #allows use of $ operator to access functions by name
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache
  inv <- x$getinverse() #function tries to retrieve inverse from x
  if(!is.null(inv)) { #checks to see if result is NULL. If it's not NULL, will return inv value from cache
    message("getting cached data")
    return(inv)
  }
  matrix_to_invert <- x$get() #if it's NULL, will calculate inverse using solve() and setinverse() to set the inverse in the input object
  inv <- solve(matrix_to_invert, ...)
  x$setinverse(inv)
  inv #return value of the inverse to the parent environment
}
