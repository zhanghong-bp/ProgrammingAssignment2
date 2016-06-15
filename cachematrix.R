## The functions use the scoping rules to cache the inverse of a matrix.
## If no changes on the matrix, it will retrieve the cached inverse matrix rather than compute it repeatedly.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinvers <- function(solve) m<<- solve
        getinvers <- function() m
        list(set = set, get = get,
             setinvers = setinvers,
             getinvers = getinvers)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## It the inverse matrix has already been calculated (and the matrix has not changed),
## then it retrieve the inverse from the cache.
## Otherwise, it calculates the inverse matrix and set the inverse matrix


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinvers()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setinvers(m)
        m
}
