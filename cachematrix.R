## Custom matrix "object" with setter, getter and setter/getter for
## inversed matrix

makeCacheMatrix <- function(x = matrix()) {
    inversedMatrix <- NULL
    set <- function(m) {
        x <<- m
        inversedMatrix <<- NULL
    }
    get <- function() x
    setInversed <- function(inversedMatrix) {
        inversedMatrix <<- inversedMatrix
    }
    getInversed <- function() inversedMatrix
    
    list(set=set, get=get, setInversed=setInversed, getInversed=getInversed)
}


## Find inverse matrix just once. Cache solution so we don't recalculate it
## each time function called

cacheSolve <- function(x, ...) {
    if (!is.null(x$getInversed())) {
        return(x$getInversed())
    }
    inversedMatrix <- solve(x$get())
    x$setInversed(inversedMatrix)
    return(inversedMatrix)
}
