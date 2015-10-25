## These functions allow to cache the inverse of a matrix


## makeCacheMatrix function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
       
        nc=ncol(x)
        nr=nrow(x)
        xInv<-matrix(NA,nrow=nr,ncol=nc)
        
        set <- function(y = matrix()) {
                x <<- y
                nc=ncol(x)
                nr=nrow(x)
                xInv<-matrix(NA,nrow=nr,ncol=nc)
        }
        
        get <- function() x
        setINV <- function(INV) xInv <<- INV
        getINV <- function() xInv
        
        list(set = set, get = get,
             setINV = setINV,
             getINV = getINV)
        

}


## cacheSolve function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve the
## inverse from the cache.

cacheSolve <- function(x, ...) {
        
        xInv=x$getINV()
        s=sum(xInv)
        
        if(!is.na(s)) {
                message("getting cached data")
                return(xInv)
        }
        
        data <- x$get()
        xInv <- solve(data, ...)
        x$setINV(xInv)
        xInv
        
}
 