## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(m = matrix()) {
        
        ## Set i_m = inverse matrix  
        i_m <- NULL
        
        set <- function(s_m) {
                m <<- s_m
                i_m <<- NULL
        }
        
        get <- function() {
                return (m)
        } 
        
        ## Set the value of inverse matrix
        
        setInverse <- function(s_inv) {
                i_m <<- s_inv
        }
        
        ## Get the value of inverse matrix
        getInverse <- function() {
                return (i_m)
        }
        
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function checks if inverse matrix has been calculated and returns inverse matrix from cache
cacheSolve <- function(c, ...) {
        
        i_m <- c$getInverse()
        
        
        if(!is.null(i_m)) {
                message("getting cached data")
                return(i_m)
        }
        
        data <- c$get()
        i_m <- solve(data)
        c$setInverse(i_m)
        return (i_m)
}
