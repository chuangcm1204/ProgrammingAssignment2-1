### Caching the Inverse of a Matrix

makeCacheMatrix <- function(x = matrix()) {
        inverseofX=solve(x)
        # initializing as empty
        m<-NULL
        # set x equal to y in the parent environment
        # clear previous calculations
        set<<-function(y){
        x<<-y
        m<<-NULL
}
# grabs a created x from the parent environment
get<-function() x
# define m variable as the inverseofx function which takes the inverse of a matrix 
setInverse<-function(inverseofX) m<<-inverseofX
# get the m from the parent environment
getInverse<-function()m
# return these functions as a list to the parent environment 
list(set=set, get=get,
    setInverse=setInverse,
    getInverse=getInverse)

}

# makeCacheMatrix Must be called before cacheSolve, otherwise cacheSolve would be error

cacheSolve <- function(x, ...) {
        # function calls getinverse from parent environment
        
        inverseofX<- x$getInverse()
        # if the inverse has already been calculated, then retrieve the inverse data
        if(!is.null(inverseofX)){
        message("getting cached data")
        return(inverseofX)
}
# if inverse is not null, then calls the makeCacheMatrix to return inverse. 
data<-x$get()
inverseofX<-solve(data)
x$setInverse(inverseofX)
return(inverseofX)
}
