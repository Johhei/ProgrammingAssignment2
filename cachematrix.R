## Inverse a matrix and store it into the cache.
##
## makeCacheVector and cacheSolve are able to inverse a matrix;
## when the inverse of a matrix is calculated, it is stored in the cache.
## When the same matrix is inversed again, the inverse will be 
## getten from the cache instead of recalculated.
## 
## Example syntax (matrixA is an invertible matrix)
## l<-makeCacheMatrix(matrixA)
## cacheSolve(l)



## makeCacheMatrix creates a list that has a function to
## 1. set the value 
## 2. get the value 
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    
    
    inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    
    setinv<-function(inverse) inv<<-inverse
    
    getinv <- function() inv
    
    list( set = set
         ,get = get
         ,setinv = setinv
         ,getinv = getinv)
    
}



## cacheSolve calculates the inverse of a matrix using the 
## list created by makeCacheMatrix
##
## cacheSolve first checks to see if the inverse has already been calculated:
## if so, it gets the inverse from the cache and skips the computation
## if not, it calculates the inverse of a matrix and stores it in the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getinv()
    
    if (!is.null(inv)) {
            message("getting cached data")
        return(inv)
    }
    
    data <- x$get()
    
    inv <- solve(data,...)
    
    x$setinv(inv)
    
    inv

}
