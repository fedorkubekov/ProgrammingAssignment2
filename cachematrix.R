## makeCacheMatrix is a function that allows to set and get both the matrix and its cached inverse.

## This creates the list of 4 functions: set, get, setInverse and getInverse.
## To store the matrix and its inverse in a way that allows them to be easily accessed or updated later.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse 
        getInverse <- function() inv 
        list(set = set, get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
}


## cacheSolve is a function that caculates the inverse of the matrix created by makeCacheMatrix.

## It first checks if the inverse has already been cached. 
## If it is available, it retrieves the cached inverse.
## If not - it calculates the inverse, caches it, and returns the inverse.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
