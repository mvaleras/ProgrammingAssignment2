##makeCacheMatrix(): This function creates a special "matrix" object that can cache its inverse,which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve(): This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        inverse <- x$getinverse()
        
        if(!is.null(inverse)) {
                message("Getting cached data...")
                return(inverse)
        }
        cacheData <- x$get()
        inverse <- solve(cacheData)
        x$setinverse(inverse)
        inverse
        
}
