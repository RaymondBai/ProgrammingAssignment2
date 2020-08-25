## makeCasheMatrix initiates two objects: matrix x and inverse matrix x_inverse.
## It then creates four functions (getters and setters) that can retrieve or 
## modify the values within objects before returning them as a list to the 
## parent environment.

## x and x_inverse are variables in the parent environment 
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


## casheSolve calls getinverse() to retrieve existing inverse matrix
## If the result is not NULL, then it returns the valid, cached inverse matrix
## If the result is NULL, then it retrieves the matrix, calculates the inverse,
## sets x_inverse to that and prints it.

cacheSolve <- function(casheMatrix, ...) {
    x_inverse <- casheMatrix$getinverse()
    if(!is.null(x_inverse)){
        message("getting cached inverse matrix")
        return(x_inverse)
    }
    x_matrix <- casheMatrix$get()
    x_inverse_calculated <- solve(x_matrix)
    casheMatrix$setinverse(x_inverse-calculated)
    x_inverse_calculated
}
