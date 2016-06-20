## This program contains two main funtions: the makeCacheMatrix and the cacheSolve.
# The makeCacheMatrix basically is used to store a matrix and its inverse. 
# In the other hand, cacheSolve is to solve the inverse of a the "x" matrix in case it has not been calculated before.


## The makeCacheMatrix function stores a list of 4 funtions: set, get, setinverse and getinverse. 
# The set function assigns values to the "x" matrix and stored it in the main function.
# The get function returns the matrix "x" stored in the main function.
# The setinverse function assigns an inverse matrix related to the "x" matrix and store it in the main function.
# The getinverse function returns the inverse matrix related to "x" matrix stored in the main function.

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set <-function(y){
                x<<-y
                inv<<-NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}

## The Cachesolve function checks the information stored in previous funtion (makeCacheMatrix)
# then if the value of the "inv" is not null its value is reported,  
# otherwise the program execute the ginv() function to get the inverse of the "x" matrix.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data<- x$get()
        inv<-solve(data, ...)
        data<- x$setinverse(inv)
        inv
}


#execution examples
a <- makeCacheMatrix(matrix(1:4,nrow=2, ncol=2))
a$get()

a$setinverse(matrix(11:14,nrow=2, ncol=2))
a$getinverse()

cacheSolve(a)

