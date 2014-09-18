## R Programming Week 3: Catching the Inverse of a Matrix

## This function creates a special "matrix" object that can
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        x_inverse <- NULL
        
        set <- function(y) {
                 
                x <<- y
                x_inverse <<- NULL
        }
        
        get <- function() x
        
        setinverse <- function(inverse) x_inverse <<-inverse        
        getinverse <- function() x_inverse        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        
        x_inverse <- x$getinverse()
        
        if (!is.null(x_inverse)) { ## Check if inverse exists
                
                message("getting inverse  from cache")                
                return(x_inverse)
                
        } else { ## Compute inverse
                
                x_inverse <- solve(x$get())                
                x$setinverse(x_inverse)
                
                return(x_inverse)
        }
}
