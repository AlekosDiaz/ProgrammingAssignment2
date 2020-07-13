## Put comments here that give an overall description of what your
## functions do

## This creates a spcial matrix thtat contains functions of: set values, get value, set the inverse value and get the inverse value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    set_inverse_matrix <- function(inverse) m <<- inverse
    get_inverse_matrix <- function() m
    list(set = set,
         get = get,
         set_inverse_matrix = set_inverse_matrix,
         get_inverse_matrix = get_inverse_matrix)
}


## This creates a function that calculates the inverse  of the special matrix, if it has calculated the same matrix before, then the saved inverse matrix is returned and is not re calculated


cacheSolve <- function(x, ...) {
    m <- x$get_inverse_matrix()
    if (!is.null(m)) {
        message("getting cached data")
        return(m) ## Return a matrix that is the inverse of 'x'
    }
    data <- x$get()
    m <- solve(data, ...)
    x$set_inverse_matrix(m)
    m
}




prueba =matrix(c(1,2,3,4,5),3,3)

prueba_especial = makeCacheMatrix(prueba)
cacheSolve(prueba_especial)



