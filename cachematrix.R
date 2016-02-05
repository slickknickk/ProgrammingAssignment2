## Below are two functions that are used to create a
## special object that stores a numeric matrix and caches the inverse of the matrix.

## The first function, `makeCacheMatrix` creates a special "matrix", which is
## really a list containing a function to
## 1.  (set) set the value of the matrix
## 2.  (get) get the value of the matrix
## 3.  (setInverse) set the value of the inverse of the matrix
## 4.  (getInverse) get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   set <- function(y) {
           x <<- y
           m <<- NULL
   }
   get <- function() x
   setInverse <- function(inverseMatrix) m <<- inverseMatrix
   getInverse <- function() m
   list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The following function calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see if the
## inverse of the matrix has already been solved. If so, it `get`s the inverse of the matrix from the
## cache and skips the computation. Otherwise, it calculates the inverse of the matrix
## and sets the value of the inverse of the matrix in the cache via the `setInverse`
## function.

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setInverse(m)
        m
}
