
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ## initialize inverse
        i <- NULL
        
        ## set the value of the matrix
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        ## get and return the value of the matrix
        get <- function() {
                x  
        }
        
        ## set the inverse value of the matrix
        setinverse <- function(matrixinverse){
                i <<- matrixinverse
        }
        
        ## get and return the inverse of the matrix
        getinverse <- function() {
                i 
        }
        
        ## return methods
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse) 
}


## calculates the inverse of the special "matrix" created 
## with the above function (makeCacheMatrix). However, it first checks to 
## see if the inverse has already been calculated. If the inverse has 
## already been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        data <- x$getinverse()
        
        ## if not null get cached data
        if(!is.null(data)) {
                message("getting cached data")
                return(data)
        }
        
        data <- x$get()
        
        ## solve(c) %*% c to invoke matrix multiplication in R.
        dataSolved <- solve(data) %*% data
        
        ## set and return the inverse
        x$setinverse(dataSolved)
        dataSolved
}
