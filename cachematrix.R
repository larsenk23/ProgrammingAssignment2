## These functions were written as part of the Coursera R Programming Course
## Week 3 Assignment, LarsenK23

##  The function MakeCasheMatrix creates a special "Matrix" that is an object 
## that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL   ##Set the value of the matrix
    }
    get <- function() x  ## Get value of the matrix
    setInverse <- function(inverse) inv <<- inverse  ##Set value of the inverse
    getInverse <- function() inv  ##Get value of the inverse
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## The following function calculates the inverse of the special "matix"
## created with the above function. However, it first checks to see if the
## inverse has already been calculated. If so, it `get`s the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse of
## the data and sets the value of the inverse in the cache via the `setInverse`
## function.


cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if (!is.null(inv)) {    ##If the matrix is not NULL
        message("getting cached data")
        return(inv)  ##the matrix inverse is returned
    }
    matrixdata <- x$get()  ##get the original matrix
    inv <- solve(matrixdata, ...)
    x$setInverse(inv)  ##set the Matrix inverse
    inv  ## Return a matrix that is the inverse of 'x'
}
