## R Function to calculate and cache the inverse of a square matrix

## Special function to create matrix and list of functions
makeCacheMatrix <- function(x = matrix()) {
        
        #Setting object for inverse of matrix to null
        m <- NULL 
        #Set function that can reset matrix and the inverse       
        set <- function(y) {
                x <<- y 
                m <<- NULL
        }
        #Get function that returns matrix
        get <- function() x
        #Set inverse function that sets inmverse equal to object m
        setinverse <- function(inverse) m <<- inverse
        #Get inverse function that returns inverse matrix
        getinverse <- function() m
        #Creating list of functions within this special function
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        
}

##Function calculates the inverse of the square matrix or returns cached value
##if the value is already stored in object m
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ##Calculate inverse of matrix of x if not cached and store it
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}

##Test matrix
test <- matrix(1:4, 2, 2)
##Input matrix into function
mymatrix <- makeCacheMatrix(test)
##Checking to see the matrix is stored
mymatrix$get()
##Checking to see the inverse if set to null
mymatrix$getinverse()
##Calculate inverse of mymatrix
cacheSolve(mymatrix)


