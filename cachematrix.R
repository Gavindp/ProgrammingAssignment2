## Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation
## caching the inverse of a matrix instead of calculating it repeatedly
## can save time


## makeCacheMatrix: creates a special "matrix" object that can
## cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL

    # set the value of the matrix.
    # make m null to show that the matrix has been set
    set <- function(y) {
        x <<- y
        m <<- NULL
    }

    # get the value of the matrix
    get <- function() {
        x
    }

    # calculate and set the inverse of the m
    setInverse <- function(solve) {
        m <<- solve
    }

    # get the inverse of the m
    getInverse <- function() {
        m
    }

    # Store the 4 functions in in a list
    list(
        set = set,
        get = get,
        setInverse = setInverse,
        getInverse = getInverse
    )
}

## cacheSolve: computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve retrieves
## the inverse from the cache.

cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of 'x'
    m <- x$getInverse()

    # if the matrix has already been inverted (indicated by m being NULL)
    # then fetch the cached matrix
    if (!is.null(m)) {
        message("getting cached matrix")
        return(m)
    }

    # if the matrix has changed, perform the solve calculation on it
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
