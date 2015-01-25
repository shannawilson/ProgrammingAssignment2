## This program is designed to provide a computationally-efficient way to 
## determine the inverse of a set of numbers stored in a matrix

## This function creates a special vector which is list containing a function to: 1) set the value
## of a matrix; 2) get the value of a matrix; 3) set the value of the inverse of the matrix; 
## 4) get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
          x <<- y
          m <<- NULL
    }
    get <-function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list (set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

## This function looks up whether the inverse of the matrix has been cached and if it has not
## been cached then it solves the inverse of the matrix. A matrix is returned to the user which
## is the inverse of the originally entered matrix

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data,...)
    x$setinverse(m)  
  ## Return a matrix that is the inverse of 'x'
   m     
}
