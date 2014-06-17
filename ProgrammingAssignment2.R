## makeCacheMatrix returns a list that:
#  1. sets value of matrix
#  2. gets value of matrix
#  3. sets value of matrix inverse
#  4. gets value of matrix inverse
####
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) m <<- inv
    getinverse <- function() m 
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}
#### cacheSolve takes a previous list of functions and computes inverse;
#  if inverse has been computed, returns cached value
#
cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
    ## Return a matrix that is the inverse of 'x'
}
#####
