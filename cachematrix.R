## Caching & Solving the Inverse of a Matrix.


## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        c <- NULL
        set <- function(y) {
                x <<- y
                c <<- NULL
        }
        get <- function() 
        setx <- function(x) x <<- solve(x)
        getx <- function() c 
        list(set=set, get=get,
             setx = setx, getx = getx)
}


## Computes the inverse of the special "matrix" 

cacheSolve <- function(x, ...) {  ## Return a matrix that is the inverse of 'x'
        y <- x$makeCacheMatrix
        if(!is.null(y)) {
                message("getting cached data")
                return(y)
        }
        cache <- x$getx()
        x <- solve(x)
        x$setx(x)
        x
        

}



