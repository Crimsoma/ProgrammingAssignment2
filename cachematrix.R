## One function makes a matrix that allows a second function to return
## objects from it or add objects onto it, in this case, inversions.
## Faster than doing computations every time desired, because if 
## an inversion is ever calculated once, the time to redo that calculation
## is reduced to whatever it takes to lookup that value in the matrix.

## Makes a 'proxy' matrix that can be used and cached with functions
## that behave in familiar matrix functional ways.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x<<-y
        m<<-NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set=set, get=get, setsolve = setsolve, getsolve=getsolve)
}


## Checks if a result has been computed and if the 'entry' is nonempty
## returns it. If empty, computes it and adds to matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if (!is.null(m)) {
        message("Reticulating cached invertible splines")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}

