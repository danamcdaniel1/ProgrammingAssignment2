
## Strategy
# part i.  use makeCacheMatrix() to create useful functions and variables
# part ii. use cacheSolve to get inverse variable in part i.  if not there, 
# part iii.  use cacheSolve for the inverse of the matrix.
# part iv. cacheSolve sets makeCacheMatrix inverse value for the future.

## makeCacheMatrix
# instantiate 'i' the variable for the inverse matrix
# instantiate list of get/set functions

makeCacheMatrix <- function(x = matrix()) { #AKA mCM
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }  ## end set
    
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    
    # mCM returns a list of functions, for reference
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve
# refers to functions, variable 'i' in makeCacheMatrix
# gets inverse 'i', if no inverse exists, solves and sets inverse

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    matrix <- x$get()
    i <- solve(matrix, ...)
    x$setinverse(i)
    i
}

# Implementation to check for correctness
q <- matrix(c(1,2,3,0,1,4,5,6,0),3)
w <- makeCacheMatrix(q)
e <- cacheSolve(w)

#Does cacheSolve give identical result as Solve()?
identical(e,solve(q))

