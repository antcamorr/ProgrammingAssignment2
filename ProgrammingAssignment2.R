## makeCacheMatrix returns a list that:
#  1. sets value of matrix
#  2. gets value of matrix
#  3. sets value of matrix inverse
#  4. gets value of matrix inverse
####
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv 
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}
#### cacheSolve takes a previous list of functions and computes inverse;
#  if inverse has been computed, returns cached value
#
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
    ## Return a matrix that is the inverse of 'x'
}
