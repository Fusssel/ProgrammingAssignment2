## The following two functions are used to inverse a matrix. It uses caching 
## of already computed matrix' in order to avoid repetive computation.

## The function 'makeCacheMatrix' is used to store and retrieve the inverted  
## matrix from the cache 

makeCacheMatrix <- function(x = matrix()) {
        
        inv_temp <- NULL
        set <- function(y) {
                x <<- y
                inv_temp <<- NULL
        }
        get <- function() x
        setinvmatrix <- function(InvMatrix) inv_temp <<- InvMatrix
        getinvmatrix <- function() inv_temp
        list(set = set, get = get,
             setinvmatrix = setinvmatrix,
             getinvmatrix = getinvmatrix)
        
}


## The function cachesolve is used to compute the inverse matrix. It first
## looks up if the solution has already been computed and if not, computes 
## the inverted matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_temp <- x$getinvmatrix()
        if(!is.null(inv_temp)) {
                message("getting cached data")
                return(inv_temp)
        }
        data <- x$get()
        inv_temp <- solve(data)
        x$setinvmatrix(inv_temp)
        inv_temp
}
