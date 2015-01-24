## Caching the Inverse of a Matrix


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

	m <- NULL
	set <- function(y){
  	x <<- y
  	m <<- NULL
}
	get <- function() x
	setmatrix <- function(solve) m <<- solve
	getmatrix <- function() m
	list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}


## This function computes the inverse of the special "matrix"

cacheSolve <- function(x, ...) {
	m<-x$getmatrix()
    	if(!is.null(m)){
      message("getting cached data")
      return(m)
    	}
    	matrix<-x$get()
    	m<-solve(matrix, ...)
    	x$setmatrix(m)
    	m


## Test : 
##  a<-makeCacheMatrix()
##  a$set(matrix(1:4,2,2))
##  cacheSolve(a)


}
