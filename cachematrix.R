## makeCacheMatrix and cacheSolve work together to cache a matrix and it's inverse. This can be used
## to cache what can be a potentially time-consuming operation -- calculating an inverse matrix.

## makeCacheMatrix takes a matrix as an argument and caches it within the makeCacheMatrix environment.
## This function works in tandem with cacheSolve() to also cache the inverse of a the cached matrix.

makeCacheMatrix <- function(x = matrix()) { # initializes x as a blank matrix
        n <- matrix()                       # initializes n as a blank matrix, this will be the inverse of x
        set <- function(y) {                # sets the value of the matrix
                x <<- y                     # assigns the argument of makeCacheMatrix$set to x within the makeCacheMatrix environment
                n <<- matrix()              # resets the inverse matrix n to a blank matrix
        }
        get <- function() x                 # simply retrieves x
        setinverse <- function(inverse) n <<- inverse   #sets the inverse n to the value of the "inverse" argument within the makeCacheMatrix environment
        getinverse <- function() n          # simply retrieves the cached value of the inverse matrix n
        list(set = set,                     # creates a list of the above functions that can be called using $
             get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}

## cacheSolve() takes an object created by makeCacheMatrix, calculates the inverse matrix, and stores it back within 
## the original makeCacheMatrix object.

cacheSolve <- function(x, ...) {            # takes a makeCacheMatrix objects as an argument
        n <- x$getinverse()                 # sets n to the current cached value (if any)
        if(!is.na(n)) {                     # if a value of n is currently cached, then this simply returns the cached value without doing any calculation
                message("getting cached data")
                return(n)
        }
        data <- x$get()                     # assigns to data the cached matrix
        n <- solve(data, ...)               # calculates the inverse of the cached matrix and assigns it to n
        x$setinverse(n)                     # uses the makeCacheMatrix object to set the inverse matrix to the newly calculated matrix n
        n                                   # returns the inverse matrix
}
