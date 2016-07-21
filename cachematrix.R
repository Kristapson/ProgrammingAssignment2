## The following contains functions for computing and caching the inverse of a matrix.

## This function creates a special matrix object that can chache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        x_inverse <- NULL ## Mark cache invalid
        set <- function(y){
                x <<- y ## Update matrix
                x_inverse <<- NULL ## Invalidate cache when updating matrix
        }
        get <- function() x 
        setinverse <- function(inverse) x_inverse <<- inverse ## Assign value to object variable
        getinverse <- function() x_inverse  ## Get value of the inverse matrix
        
        list(set = set, get = get,      ## Form the returned list
             setinverse = setinverse,
             getinverse = getinverse) 
}

## This function computes the inverse of the special matrix object created by the 
## makeCacheMatrix function. If the inverse has been calculated already, then the 
## cacheSolve function retrieves the inverse from cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        if(is.null(x$getinverse())){ ## Check if cache is invalid
                m <- x$get() ## Retrieve matrix
                m_inverse <- solve(m) ## Compute inverse matrix
                x$setinverse(m_inverse) ## Save inverse matrix in cashe 
               
                m_inverse ## Return inverse matrix
        }else{
                x$getinverse() ## If already calculated, get the inverse matrix
                
        }
}
