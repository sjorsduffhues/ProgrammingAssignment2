## makeCacheMatrix and cacheSolve can be used to compute the inverse of a 
##(invertible) matrix without computing the inverse in case this is not necessary

## makeCacheMatrix creates an object that can cache its inverse, so it can be
## used by the cacheSolve function if the inverse is already computed and the 
## matrix is not changed (instead of computing the inverse every time).

makeCacheMatrix <- function(x = matrix()) {
	inv<-NULL

	get<-function()x
	set<-function(y){
		x<<-y
		inv<<-NULL
	}

	getinv<-function()inv
	setinv<-function(solve)inv<<-solve

	list(get=get, set=set, getinv=getinv, setinv=setinv)
}

##This function returns the inverse of the matrix that is created by makeCacheMatrix.
##If the inverse for this matrix is already computed, it retrieves the inverse form  the cache.

cacheSolve <- function(x, ...) {
   ##get hte cached inverse matrix
	inv<-x$getinv()
	
   ##if the cached inverse matrix exists, return it
	if(!is.null(inv)){
		message("getting cached data")
		return(inv)
	}

   ##if the cached inverse matrix does not exist, create it and set it as the new inv for this matrix
	matrix<-x$get()
	inv<-solve(matrix,...)
	x$setinv(inv)
	return(inv)
}
