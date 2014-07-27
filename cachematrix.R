## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        #initialize inverse
        i <- NULL
        
        #set the value of the matrix
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        #get and return the value of the matrix
        get <- function() {
                x  
        }
        
        #set the inverse value of the matrix
        setinverse <- function(matrixinverse){
                i <<- matrixinverse
        }
        
        #get and return the inverse of the matrix
        getinverse <- function() {
                i 
        }
        
        #return methods
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse) 
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        data <- x$getinverse()
        
        if(!is.null(data)) {
                message("getting cached data")
                return(data)
        }
        
        data <- x$get()
        
        #solve(c) %*% c to invoke matrix multiplication in R.
        dataSolved <- solve(data) %*% data
        
        #set and return the inverse
        x$setinverse(dataSolved)
        dataSolved
}
