## Functions create a matrix object with a storable inverse.  Inverse is saved
## to reduce computation for large matricies and often called inverses.
##
## makeCacheMatrix will create matrix object
## cacheSolve will calculate and store inverse to objects created in makeCacheMatrix



## makeCacheMatrix will create matrix object.
##
## Set matrix by calling objectName <- makeCacheMatrix(baseMatrix)
## Set new matrix with objectName$set(newMatrix) This clears the existing inverse
## recall matrix with objectName$get()
## set matrix inverse with objectName$setInv()
## recall inverse with objectName$getInv()

makeCacheMatrix <- function(x = matrix()) {
	# initialize by ensuring clear inverse
	inv <- NULL
	
	# set new base matrix y to object then clear any existing inverse
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	
	# get allows you to get stored matrix
	get <- function() x
	# setInv allows for storage of inverse by cacheSolve function (defined below)
	setInv <- function(inverseMat) inv <<- inverseMat
	# getInv used to recall inverse matrix
	getInv <- function() inv
	
	# sets list of callable sub functions
	list( set = set,
		  get = get,
		  setInv = setInv,
		  getInv = getInv)
		  
}


## cacheSolve will solve for inverse of requested matrixObject created
## with makeCacheMatrix.
##
## call cacheSolve(matrixObject) to store inverse to matrixObject data

cacheSolve <- function(x, ...) {
	# get current inverse matrix from stored object
	# will return a matrix if there is and NULL if not
	inv <- x$getInv()
	
	# if the inverse is there (not NULL) then return cached value and say so
	if(!is.null(inv)){
		message('getting cached data')
		return(inv) #exits function here with existing inverse
	}
	# no current inverse matrix, continue:
	
	# get stored matrix of which you require inverse
	mat <- x$get()
	# calculate inverse of matrix
	inverseMat <- solve(mat)
	# store inverse with source object method setInv
	x$setInv(inverseMat)
	
	## Return a matrix that is the inverse of 'x'
	return(inverseMat)
}
