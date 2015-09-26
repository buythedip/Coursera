makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  # function that returns the matrix stored in the main function
  get<-function() x
  # function that changes the matrix value
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  # creates a list of the functions that comprise the main function
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  # verifies that m is not NULL
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  # calculate and store inverse matrix
  datos<-x$get()
  m<-solve(datos, ...)
  x$setmatrix(m)
  m
}
