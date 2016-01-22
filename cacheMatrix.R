# The input should look like ¡¡ > matrix(c(1,0,0,-1),nrow=2,ncol=2) !!
# Then, if I wanted the inverse of matrix (1 0) I'd write in the console ¡¡ > cacheSolve(makeCacheMatrix(matrix(c(1,0,0,-1),nrow=2,ncol=2))) !!
#										                      (0 1)


makeCacheMatrix <- function(x = matrix()){
		I <- NULL
        set <- function(y) {
                x <<- y
                I <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) I <<- inverse
        getInverse <- function() I
        list(set = set, get = get,
                setInverse = setInverse,
                getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
        I <- x$getInverse()
        if(!is.null(I)) {
                message("getting cached data")
                return(I)
        }
        data <- x$get()
        I <- solve(data, ...)
        x$setInverse(I)
        I
}
