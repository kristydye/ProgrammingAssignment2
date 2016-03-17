## to play with these,
## (1) create a matrix
##     > set.seed(123)
##     > mx <- matrix(rnorm(16), nrow=4, ncol=4)
## (2) use the matrix
##     > a <- makeCacheMatrix(mx)
##     > b <- cacheSolve(a)
## (3) see the cache in action
##     > c <- cacheSolve(a) ... it should tell you it found the cached inverse

## makeCacheMatrix returns a set of functions for for getting and setting a matrix, and setting and getting
##the inverse of the matrix. Input is a matrix.

makeCacheMatrix <- function(x = matrix()) {
    invs <- NULL
    set <- function(y) {
        x <<- y
        invs <<- NULL
    }
    get <- function() x
    setinvs <- function(solve) invs <<- solve
    getinvs <- function() invs
    list(set = set, get = get, setinvs = setinvs, getinvs = getinvs)
}


## cacheSolve accepts an object of type makeCacheMatrix, will check if the matrix in it is invertable. If it is,
##it will check if this object's matrix has already been inverted; if so, it will return the inverse;
##if not, it wil invert (solve), store that value in the object cache, and return the inverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    if(det(x$get())!=0) {
        invs <- x$getinvs()
        if(!is.null(invs)) message("found cached inverse")
        else {
            data <- x$get()
            invs <- solve(data)
            x$setinvs(invs)
        }
        return(invs)
    }
    else message("ermahgerd not invertable")
}
