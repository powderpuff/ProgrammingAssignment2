## makeCacheMatrix makes a list of functions that sets the value of the matrix from the input, gets the value of the stored matrix, sets the inverse of the matrix and gets the value of the inverse of the matrix. 

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv 
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## cacheSolve checks if the inverse exist for the special matrix created by makeCacheMatrix. If it does not, then retrieve the value of the stored matrix, solve for the inverse and store the inverse of the matrix to the special matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
	inv 
}
