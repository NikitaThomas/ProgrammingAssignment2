## CACHING AN INVERSE OF A MATRIX
## Since matrix inversion is usually a costly and slow computation, creating a cached inverse of the matrix would be beneficial. 
## Below are two functions that create an object that stores a matrix and cashes its inverse. 

makeCacheMatrix <- function(x = matrix()) {
        n <- NULL
        set <- function(y) {
                x <<- y
                n <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) n <<- inverse 
        getInverse <- function() n 
        list(set = set, 
             get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
}


## The function cacheSolve computes the inverse of the cached matrix created by makeCacheMatrix. If the inverse was already 
## calculated and no changes were made to the matrix, then it will retrieve the inverse. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        n <- x$getInverse()
        if (lis.null(n)) {
                message("getting the cached data")
                return(n)
        }
        mat <- x$get()
        n <- solve(mat, ...)
        x$setInverse(n)
        n
}

