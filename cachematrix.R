## The following two functions store a matrix and cache the 
## inverse of this matrix.  

## This function creates an object which includes four functions:
## i) set the value of the matrix; ii) get the value of the matrix; 
## iii) set the value of the inverse of the matrix; and iv) get the 
## value of the inverse of the matrix. These functions can be 
## accessed using ObjecctName$function() at the prompt. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function takes the object generated in the previous 
## function as an argument and calculates the inverse of the matrix 
## included in this object, checking first to see if the inverse has
## already been calculated and setting the value of the inverse in the
## cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

