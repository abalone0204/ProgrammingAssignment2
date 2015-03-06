## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Assuem that all of our input matrix are nonsingular
# Create a cache of inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    inv_x <- NULL
    set <- function(y) {
        x <<- y
        inv_x <<- NULL
    }
    get <- function() x
    set_inverse <- function(x)  inv_x <<- solve(x,1)
    get_inverse <- function() inv_x
    list(set = set, get = get, 
        set_inverse = set_inverse,
        get_inverse = get_inverse)
}


## Write a short comment describing this function
# Check if there is a inverse matrix has been created,
# the following function will return the value from cache
# Or, it would calculate and cache it!
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_x <- x$get_inverse()
        if(!is.null(inv_x)) {
            message("Getting cached data")
            return(inv_x)
        }
        data <- x$get()
        inv_x <- solve(data, 1)
        x$set_inverse(inv_x)
        inv_x
}
