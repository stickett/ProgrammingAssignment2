## makeCacheMatrix stores data about a given matrix in a list format.  cacheSolve is then used to solve for the inverse of the matrix given in makeCacheMatrix.  cacheSolve uses lexical scoping and tests to see if the matrix has been solved already by testing to see if its inverse, m, is null.  If cacheSolve has not been used before, m is null and cacheSolve solves for the inverse of the matrix.  If the matrix has been solved for and m isn't null, cacheSolve uses lexical scoping to find the stored inverse matrix in the parent environment and returns it, saving computation time.


## This function uses a matrix as an argument and is used to assign a list to a variable.  First, it sets m, the inverse of the matrix to null.  Then it stores the matrix argument in get which is later used in cacheSolve to retrieve the matrix.  Next it stores the value of the inverse in m.  Finally, it shows the list information.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) m <<- Inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function solves for the inverse of the given matrix, but first checks to see if it has already been stored as m.  It retrieves m using the getInverse function.  Then it tests to see if m is null.  If it is not null (i.e. it's been tested before), it returns m, the inverse, stored from before.  If it is null, it uses the get function to retrieve matrix and store it in data.  It then uses the solve function on data to find the inverse.  Finally, it uses the set function to store the value of the inverse in m using the setInverse function.

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m        
}

# Try the following to see if it works:
# t <- makeCacheMatrix (matrix(1:4,2,2))
# cacheSolve(t)
# cacheSolve(t)
# tada!!