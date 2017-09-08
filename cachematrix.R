## These two functions allow the calculation of the inverse of a square matrix
## using a cached value if available

## makeCacheMatrix creates the matrix object that is used for the inverse calc
## it contains getters and setters for storing the matrix in a cache that
## utilises the containing environment

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverseMatrix <- function(inv) i <<- inv
    getInverseMatrix <- function() i
    list(set = set,
         get = get,
         setInverseMatrix = setInverseMatrix,
         getInverseMatrix = getInverseMatrix)
}

## cacheSolve checks if a cached inverse matrix is already available. If it is,
## then the cache is used, otherwise it performs the inverse calculation
## with the solve() function

cacheSolve <- function(x, ...) {
    i <- x$getInverseMatrix()
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverseMatrix(i)
    i
}
