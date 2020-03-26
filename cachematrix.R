

#The first function, makeCacheMatrix creates a inverse matrix 


makeCacheMatrix <- function(x = matrix()) { #set the value of the matrix
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x  #get the value of the matrix
        setinverse <- function(inverse) i <<- inverse #set the value of the inverse
        getinverse <- function() i #get the value of the inverse
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
