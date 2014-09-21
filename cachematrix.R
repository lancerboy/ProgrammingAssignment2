# This pair of functions cache the inverse of a matrix
# If the inverse has already been calculated and the matrix is not changed,
# it will retrieve the inverse from the cache

# makeCacheMatrix creates a list which contain functions to
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse of the matrix
# 4. Get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
        z <<- x
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

# cacheSolve checks if the inverse has already been calculated AND
# if the matrix has not changed 
# If both conditions are TRUE, it returns the cached data
# If either condition is not TRUE, it solves for the matrix provided
# AND saves the new matrix as the base comparison matri

cacheSolve <- function(x = matrix(), ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv) && identical(x$get(),z)) {
        message("getting cached data")
        return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix)
    z <<- matrix
    x$setinv(inv)
    inv
}
