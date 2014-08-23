## These functions will create a matrix with object like qualities,
## allow you to manipulate the matrix contained within the object,
## find the inverse of the matrix (assuming the matrix is inversible),
## and store this value to save computation time if the function to calculate
## the inverse is called again on the same matrix.

## makeCacheMatrix is obviously built off the provided makeVector function.
## It takes a matrix as input and provides methods and other "attributes",
## to retrieve or set values to those "attributes".

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL   ## variable where the inverse is cached, NULL upon instantiation
    
    set <- function(y) {
        x <<- y    ## method to reset the stored matrix without re-calling
        m <<- NULL ## the entire function.  Resets the cache to NULL.
    }
    get <- function() x  ## returns the matrix
    
    setinverse <- function(inverse) m <<- inverse   ## Caches the matrix inverse
                                                    ## to the variable m created
                                                    ## in the parent environment
                                                    ## (ie in the function where 
                                                    ## "setinvere" is defined)
    
    getinverse <- function() m      ## returns the matrix inverse
    
    list(set = set, get = get,      ## creates a list allowing the user to 
         setinverse = setinverse,   ## access the methods of the matrix "object"
         getinverse = getinverse)
}


## cacheSolve is a utility function that will calculate the inverse of a matrix,
## unless it has prevously been calculated and stored in m.
## It too is obvisouly based on the cacheVector example provided.
## It takes the list object created by makeCacheMatrix to access the data,
## check the stored matrix inverse data, and calculate the inverse
## if a solution hasn't already been calculated.

cacheSolve <- function(x, ...) {
    m <- x$getinverse() ## retrieves the matrix inverse from the list object x
    
    if(!is.null(m)) {   ##if the inverse has already been computed, thus not NULL
        message("getting cached data")  ## display a descriptive message
        return(m)                       ## return the stored matrix inverse
    }                                   ## and exit the function
    
    ## This section is only reached if the inverse has not been
    ## previously calculated.
    
    data <- x$get()  ## retrieve the matrix from the list object x
    
    m <- solve(data, ...)  ## compute the inverse of the matrix
    
    x$setinverse(m)       ## cache the inverse back into the list object x
    
    m                     ## return the calculated inverse
}
