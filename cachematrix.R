## They're two functions that calculate and cache the inverse of a matrix, than in case that matrix it's already stored the function will 
## retrieve it instead of calculate again

## The first function creates a void matrix than gets its argument calculates its inverse
## and save it in a matrix called inv

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL 
        }
        
        get <- function() x
        setInv <- function(solve) inv <<- solve
        getInv <- function() inv
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## This function gets its argunment and check if its inverse has already been calculated, if so it'll retrieve if, otherwise it'll calculate

cacheSolve <- function(x, ...) {
        
        inv <- x$getInv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInv(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}
