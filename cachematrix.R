#makeCacheMatrix: This function creates a special "matrix" object that caches its inverse.
#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix.

makeCacheMatrix <- function(x = matrix()) {
  
  m<-NULL # Set the value of the vector
  set<-function(y){
  
  x<<-y
  m<<-NULL
    
  }
  
  get<-function() x #get the value of the vector
  
  setmatrix<-function(solve) m<<- solve #set the value of the matrix
  getmatrix<-function() m #get the value of the matrix
  
  list(set=set, get=get,setmatrix=setmatrix,getmatrix=getmatrix)
  
}

cacheSolve <- function(x=matrix(), ...) {
  
  m<-x$getmatrix()
  if(!is.null(m)){ #if the matrix has not already been calculated, notify and then calculate, otherwise skip
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...) #calculates the matrix of the special "vector" created with makeCacheMatrix function
  x$setmatrix(m) #sets the value of the matrix
  m
  
}
