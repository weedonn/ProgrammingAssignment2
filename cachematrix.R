## The 2 functions defined below create and maintain an special "square matrix"
## object that contains functions and local data to compute and save the inverse
## of the supplied square matrix.
## The inverse of the matrix is calculated using the solve() function in R.
## For example,
## if X is a square invertible matrix, then solve(X) returns its inverse.

## The makeCacheMatrix() function takes an optional matrix object as it's first
## parameter and returns a special "makeCacheMatrix" list object.
## The 4 makeCacheMatrix functions are designed to maintain data consistency.
## That is, if the source matrix or the inverse matrix are updated via these
#  functions, then the other matrix is re-calculated to maintain consistency
## between the matricies and the makeCacheMatrix object.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                if (identical(y,x)) {
                        message("no change in matrix")
                } else {
                        x <<- y
                        m <<- solve(y)
                }
        }
        get <- function() x
        setInv <- function(inv) {
                if (identical(inv,m)) {
                        message("no change in inverse")
                } else {
                        m <<- inv
                        x <<- solve(inv)
                }
        }
        getInv <- function() m
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## The cacheSolve() function takes a mandatory "makeCacheMatrix" list object as
## it's first parameter. If the matrix inverse has been cached, then a message
## "getting cached data" is displayed and the cached matrix inverse is returned.
## Otherwise the matrix inverse is calculated, cached, and then returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setInv(m)
        m
}
