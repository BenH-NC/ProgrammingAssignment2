## Put comments here that give an overall description of what your
## functions do.....
## The first function makeCacheMatrix creates a special "matrix" object that 
## can cache its inverse.
## The second function cacheSolve computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the inverse has already been
## calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.
##

## Write a short comment describing this function
## The function will accept one matrix (x) and will return the inverse of ## the matrix so that we can use it with the cacheSolve function below


makeCacheMatrix <- function(x = matrix()) {

        inver <- NULL
        set <- function(y) {
                x <<- y
                inver <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inver <<- inverse
        getinverse <- function() inver
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## If the inverse has already been calculated (and the matrix
        ## has not changed), then the cachesolve should retrieve 
        ## the inverse from the cache.
        inver <- x$getinverse()
        if(!is.null(inver)) {
                message("getting cached data")
                return(inver)
        }
        data <- x$get()
        inver <- solve(data, ...)
        x$setinverse(inver)
        return(inver)

}
