## As proposed in the given example, this function initialize the matrix that can cache its inverse  
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x<<- y
                inv <<- NULL
        }
        get <- function() x
        setinversa <- function(inversa) inv <<- inversa
        getinversa <- function() inv
        list(set = set, get = get, setinversa = setinversa, getinversa = getinversa)
}


## This function calculates the inverse of the matrix created with the above function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'   
        ## first checks if the inverse is already calculated
        ## if it is already calculated prints the message "getting cached data" and then return the inverse
        inv <- x$getinversa()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }      
        
        ## otherwise calculates the inverse via the setinversa function
        matriz <- x$get()
        inv <- solve(matriz, ...)
        x$setinversa(inv)
        inv
}
