## makeCacheMatrix is a function that stores a list of functions that can create and store the inverse of an input matrix. casheSolve then
## takes the output (object in which makeCacheMatrix is stored) and determines if the inverse has already been calculated. If the inverse
## matrix is already stored cachSolve returns the it, if it is not already stored cacheSolve calculates and returns the inverse matrix.

## makeCacheMatrix is a function that stores a list of functions. The functions within makeCacheMatrix can store values of an input matrix (set)
## return the values of a set matrix (get), set the inverse of the input matrix (setinverse) and return the inverse values (getinverse)

makeCacheMatrix <- function(x = matrix()) { # creating a function that stores a list of functions and takes an array (matrix) as an argument
  m <- NULL # sets the inverse to the null value when using the main function set the matrix (and gives it somewhere to store the inverse)
  set <- function(y) { # function that can be used to set/change the matrix stored in the main function (ie. with makeCacheMatrix$set())
    x <<- y # substitutes the matrix x with y in the main function
    m <<- NULL # restores the inverse to the null value when using set to change the matrix
  }
  get <- function() x # function to return the matrix stored in the main function
  setinverse <- function(solve) m <<- solve # stores the value of the input in variable m (for the inverse) into the main function
  getinverse <- function() m # return the value of the input in variable m (the inverse) from the main function
  list(set = set, get = get, # store the 4 functions (get, set, setinverse, getinverse) in the main function
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve is a function that takes an object where makeCacheMatix is stored, checks for a stored inverse and calculates the inverse if
## one is not already stored.

cacheSolve <- function(x, ...) { # the input of function is the object where makeCacheMatrix is stored
  m <- x$getinverse() # checks to see if there is an existing value for m stored in x in getinverse
  if(!is.null(m)) {
    message("getting cached data")
    return(m) # this if statement will return the message indicated and the value of m if m has already been stored (ie. is not null)
  }
  data <- x$get() # retrieves the matrix stored in makeCacheMatrix (ie. x)
  m <- solve(data, ...) # calculates the inverse for the matrix 
  x$setinverse(m) # stores the inverse for the matrix in setinverse
  m # returns the inverse matrix
}

