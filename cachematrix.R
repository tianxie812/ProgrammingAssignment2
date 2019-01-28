## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
         Inv <- NULL
         set <- function(y) {
         x <<- y
         Inv<<- Null
         }
        
         get <- function() x
         setInverse <- function(Inverse) Inv <<- Inverse
         getInverse <- function() inv
         list(set = set, get = get,
              setInverse = setInverse,
              getInverse = getInverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed)
##Then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      Inv <- x$getInverse()
        if(!is.null(Inv)) {  
                message("getting cached data")  
        }
        data <- x$get() 
        Inv <- solve(data, ...)
        x$setInverse(inv)
        Inv
}
