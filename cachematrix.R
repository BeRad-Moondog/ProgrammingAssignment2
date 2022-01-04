makeCacheMatrix <- function(x = matrix()) {
        # x is a square invertible matrix
        #This function returns a list of functions for setting and getting the special matrix m.
        #Also the returned list provides functions for getting and setting the inverse of the matrix m
        #The returned list is used as an input into cacheSolve(x)
        
        inv = NULL
        set = function(y) {
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
        # x is the output of makeCacheMatrix()
        #This function returns the inverse of the input special matrix created by makeCacheMatrix()
        inv = x$getinv()
        
        if (!is.null(inv)){
                return(inv)  #skip calculation of the inverse it if has already been calculated
        }else{
                mat.data = x$get()
                inv = solve(mat.data, ...)
                 x$setinv(inv)
        }
        
        return(inv)
}

##Test Code:
## x<-matrix(rnorm(16),4,4)
## cacheSolve(makeCacheMatrix(x)) %*% x     ###Should provide a resonable approximation of the Identity Matrix
