## makeCacheMatrix and cacheSolve return a matrix inversion
## result is produced from cached prior result or calculated if no prior exists
## 
## simple modification of mean example with matrix invert instead

makeCacheMatrix <- function(x = matrix()) {
        invt <- NULL
        set <- function(y) {
                x <<- y
                invt <<- NULL
        }
        get <- function() x
        setinvt <- function(inverse) invt <<- inverse
        getinvt <- function() invt
        list(set = set, get = get,
             setinvt = setinvt,
             getinvt = getinvt)
}


## Invert a square matrix or return a cached calcuation 
## simple modification of mean example using with solve instead

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invt <- x$getinvt()
        if(!is.null(invt)) {
                message("getting cached data")
                return(invt)
        }
        data <- x$get()
        invt <- solve(data)
        x$setinvt(invt)
        invt
}
