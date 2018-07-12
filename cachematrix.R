## makeCacheMatrix will create a special type of matrix where it's inverse can be cached.

## cacheSolve will retrieve or calculate the inverse of the matrix received as an
## argument, provided that the makeCacheMatrix has been applied to the matrix
## first.

## The following function creates a list of functions that:
## 1. Sets the values of the matrix
## 2. Gets the values of the matrix
## 3. Sets the inverse of the matrix
## 4. Gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setSolve <- function(solve) s <<- solve
    getSolve <- function() s
    list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}


## The following function first gets the solved matrix from cache.
## If the solved matrix has been cached the function returns it.
## If the solved matrix hasn't been cached (is NULL) then the function
## will solve the matrix, cache it and return the solved matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    s <- x$getSolve()
    if(!is.null(s)) return(s)
    data <- x$get()
    s <- solve(data, ...)
    x$setSolve(s)
    s
}
