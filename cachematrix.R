## This combination of functions allows a matrix inverse to be stored
## in cache in a class that encompasses the matrix.

## Class that encompasses a matrix by adding a inverse function to it

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setSolve <- function(inverse) inv <<- inverse
  getSolve <- function() inv
  list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
  
}


## This function solves the encompassed matrix for an inverse.
## If the matrix has already had its inverse found, it will be just be very quick

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getSolve()
  if(is.null(inv)){
    solution <- solve(x$get(),...)
    x$setSolve(solution)
    inv <- solution
  } else{
    print("Already solved, returning cached...")
    return(inv)
  }
  inv
}
