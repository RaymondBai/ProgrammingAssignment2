## Put comments here that give an overall description of what your
## functions do

## matrix x and x_inverse are variables in the parent environment 
## (makeCasheMatrix) and can be modified with <<- inside setters

makeCacheMatrix <- function(x = matrix()) {
    x_inverse <- NULL
    
    set <- function(y){
        x <<- y
        x_inverse <<- NULL
    }
    get <- function() x ## x is not defined inside get(). By lexical scoping, 
                        ## it is retrieved from the parent environment
    setinverse <- function(inverse) x_inverse <<- inverse
    getinverse <- function() x_inverse ## lexical scoping feature
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}.


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    x_inverse <- x$getinverse()
    if(!is.null(x_inverse)){
        message("getting cached inverse matrix")
        return(x_inverse)
    }
    x_matrix <- x$get()
    x_inverse <- solve(x_matrix)
    x$setinverse(x_inverse)
    x_inverse
}
