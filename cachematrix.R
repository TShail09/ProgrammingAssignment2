## CACHING THE INVERSE OF A MATRIX

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set_matrix<-function(y) {
        x<<-y
        inv<<-NULL
    }
    get_matrix<- function() x
    setinv<-function(inverse) inv<<-inverse
    getinv<-function() inv
    list(set_matrix=set_matrix,get_matrix=get_matrix,setinv=setinv,getinv=getinv)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then cacheSolve should retrieve the inverse from the
## cache

cacheSolve <- function(x, ...) {
    inv<-x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mtrx<-x$get_matrix()
    inv<-solve(mtrx, ...)
    x$setinv(inv)
    inv
}
