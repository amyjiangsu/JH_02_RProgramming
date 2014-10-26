# Matrix inversion is usually a costly computation, hence there are some
# benefit to caching the inverse of a matrix rather than computing it repeatedly.
# The following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing the functions to
# 1. set the value of a matrix
# 2. get the value of a matrix
# 3. set the value of the inverse of a matrix
# 4. get the value of the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


# cacheSolve function returns the inverse of a matrix.  If first checks if
# the inverse has already been computered.  If so, it gets the result and 
# skips the computation.  If not, it computes the invers, and sets the inverse value
# via setinverse function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}

# Sample run:
# > x <- matrix(1:4,2,2)
# > m  <- makeCacheMatrix(x)
# > m$get()
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > cacheSolve(m)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(m)
# getting cached data
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
