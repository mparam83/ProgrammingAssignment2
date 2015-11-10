## Caching the inverse of a matrix

# Here I write a pair of functions that cache the inverse of a matrix to save on computation time.


# The makeCacheMatrix function creates a special "matrix" object that can cache its inverse. It takes an input, x, that is a square invertible matrix. It returns a list of four functions that set the matrix, get the matrix, set the inverse and get the inverse. These four functions are used as inputs in the second function cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
        myinverse <- NULL # initialise an empty vector to store the calculated inverse
        set <- function(y) {
            x <<- y # the <<- operator can be used to assign a value to an object in an environment that is different from the current environment.
            myinverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) myinverse <<- solve
        getinverse <- function() myinverse
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse) # a list of output functions

}


# The cacheSolve function calculates the inverse of the special "matrix" created with the above function. It takes the output of makeCacheMatrix as its input. It first checks to see if the inverse of the entered matrix has already been calculated. If so, rather than calculating it again, it return the inverse from the cache. If it has been calculated already, the function calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinverse function. Then it returns the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        myinverse <- x$getinverse()
        # use an if clause to check if the inverse has already been calculated
        if(!is.null(myinverse)) { 
            message("getting cached data") # display using cache message
            return(myinverse) # return the cached solution
        }
        # if not already calculated, calculate the inverse
        mydata <- x$get() # store the inputted matrix into mydata
        myinverse <- solve(mydata, ...) # use solve on mydata to calculate the inverse
        x$setinverse(myinverse) # set myinverse to the calculated solution
        return(myinverse) # return myinverse, which is the inverse of the matrix
}

# Testing
a <- matrix(1:4, 2, 2)
b <- makeCacheMatrix(a)
cacheSolve(b)

c <- matrix(runif(25, 1, 50), 5, 5)
d <- makeCacheMatrix(c)
cacheSolve(d)
cacheSolve(d)

# END #
