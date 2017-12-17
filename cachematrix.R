## This is writen for the third week assignment of RProgramming

## the first function puts a matrix in the cache

makeCacheMatrix<- function(x=matrix()){
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function()x
  setinv<-function(inverse)inv<<-inverse
  getinv<-function()inv
  list(set=set, get=get,setinv=setinv, getinv=getinv)
}


## This function returns a matrix that is the inverse of 'x'

cachesolve <-function(x, ...) {
  inv<-x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat.data <-x$get()
  inv<-solve(mat.data, ...)
  x$setinv(inv)
  return(inv)
}
