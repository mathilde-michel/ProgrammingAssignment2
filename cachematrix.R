## Functions that creates for each matrix a list containing the matrix 
## and it inverse (if already computed) and that enable to check through
## these lists whether or not the inverse has already been computed 
## in order not to compute it another time.

## Function that creates for each matrix a list containing 
## the matrix and its inverse 'if already computed). It thus enables 
## storing & calling matrix to be inversed and the corresponding inverse.

makeCacheMatrix <- function(x = matrix()) {
       # initializing m.
        m<-NULL
        
        #defining functions storing and calling the matrix to be inversed.
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function()x
        
        #defining functions storing and calling the inverse of the matrix.       
        setinverse<-function(inverse)m<<-inverse
        getinverse<-function() m
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Functions that checks if the inverse has already been calculated:
## If it has it displays the calculation already made.
## If not it calculates the inverse.
## works only on matrixes already computed through makeCacheMatrix.

cacheSolve <- function(x, ...) {
        #checking if the calculation has already been done
        m<-x$getinverse()
        if(!is.null(m)) {
                message("getting cache data")
                return(m)
        }
        
        #inversing a matrix if not done previously
        data<-x$get()
        m<-solve(data,...)
        x$setinverse(m)
        m

}
