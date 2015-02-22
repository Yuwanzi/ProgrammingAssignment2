#creates a list that contains 4 member functions: set, get, setInv
#and getInv. 


makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setsolve<-function(solve) m<<- solve
        getsolve<-function() m
        list(set=set, get=get,
             setsolve=setsolve,
             getsolve=getsolve)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getsolve()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        matrix<-x$get()
        m<-solve(matrix, ...)
        x$setsolve(m)
        m
}

