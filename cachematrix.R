## Assignment2, here are two functions for taking the inverse
## of given matrix using cache.

## makeCacheMatrix function gets the matrix value and set the value.
## and getSolve and setSolve are to get and set the INVERSE correspondingly

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
                        x <<- y
                        i <<- NULL
                        }
    get <- function() x
    setSolve <- function(inverse) i <<- inverse
    getSolve <- function() i
    list(set = set, get = get,setSolve = setSolve,getSolve = getSolve)
                                            }

## cacheSolve function checks if the inverse has already been calculated
## if not it calculates and writes to cache, 
## if already calculated then just returns it from the cache with message "Getting the Cached Matrix"

cacheSolve <- function(x, ...) {
           i <- x$getSolve()
           if(!is.null(i)) {
                             message("Getting the Cached Matrix")
                             return(i)
                            }
           data <- x$get()
           i <- solve(data, ...)
           x$setSolve(i)
           i
           ## Return a matrix that is the inverse of 'x', which is i (inverse)
                                }