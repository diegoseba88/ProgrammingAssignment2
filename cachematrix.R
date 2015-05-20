## These 2 functions combine to produce faster results via reducing the amount of calculations R needs to perform. In other words, 
##      If R has already done something, we can advise "him" to store a RESULT and pick it up later for future computations. 

## This 1st Function defines a Matrix object, and stores a few parameters in Cache

makeCacheMatrix <- function(x = matrix()) {
m <- NULL # Applies NULL value to variable m
        set <- function(y) { #Creates annonymous function with one variable y named set
                x <<- y
                m <<- NULL # This function applies values to x and m from outside the environment.
        }
        get <- function () {
                x
        }
        setsolve <- function(solve) {
                m <<- solve
        } 
        getsolve <- function() {
                m
        }
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve) # Result is a List object with the result of the previuosly define variables as functions.
}


## This 2nd function will solve the Matrix if its result is not on Cache, otherwise, it will just pick up the result from it avoiding tedius and repetitive computations.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)    #If m is not empty it means the Data is in Cache already, so it doesn't need to be recalculated.
        }
        data <- x$get()
        m <- solve(data, ...) #Otherwise, m is calculated
        x$setsolve(m)
        m     # Return a matrix that is the inverse of 'x'
}
