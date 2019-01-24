###The first function set and get the inverse of a square matrix
###The second function calculates the inverse of the matrix created by
#the first matrix. Before solving for inverse, it first checks to see
##if it was already solved for. If yes, then it returns the cached inverse.

## function makeCacheMatrix does the following:
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse

makeCacheMatrix<-function(x=matrix()){
  inv<-matrix(NA,nrow=dim(x)[1],ncol=dim(x)[2])
  set<-function(y){
    x<<-y
    inv<<-matrix(NA,nrow=dim(x)[1],ncol=dim(x)[2])
  }
  get<-function() x
  setinv<-function(solve) inv<<-solve
  getinv<-function() inv
  
  list(set=set,get=get,
       setinv=setinv, getinv=getinv)
}


## The following function calculates the inverse of the special
# matrix created with the above function

cacheSolve<-function(x,...){
  inv<-x$getinv()
  if(!all(is.na(inv))){
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
