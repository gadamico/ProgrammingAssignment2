## The first function will set up the list and the cache; the second will do the matrix-inverse calculation using "solve()".

## makeCacheMatrix creates four functions to set up the cache for cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
i<-NULL
set<-function(y){
        x<<-y
        i<<-NULL
        }
get<-function() x
setinverse<-function(inv) i<<-inv
getinverse<-function() i
list(set=set, get=get,
        setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve returns the inverse of the inputted matrix.

cacheSolve <- function(x, ...) {
        i<-x$getinverse()
        if(!is.null(i)){
                message("getting cached data")
                return (i)
                }
        data<-x$get()
        i<-solve(data, ...)
        x$setinverse(i)
        i
}
