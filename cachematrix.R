##makeCacheMatrix is a function that contains four other functions and takes x = matrix() as an argument. It will be used as an 
##argument for the function cacheSolve, which will call upon the internal functions. These internal functions work on the 
##matrix that makeCacheMatrix uses as an argument. The overall goal is to calculate the inverse matrix of x = matrix(), and 
##store it so it can be re-used without re-calculating.

makeCacheMatrix <- function(x = matrix()) {
	z <- NULL #z will represent the inverse matrix. It starts as NULL to represent there being no cached data
	set_matrix <- function(y) { ##set_matrix can change the matrix being used by makeCacheMatrix (x = matrix()) to another matrix.
	                            ##This resets the value of z to NULL so cacheSolve will know to not used cached data, and instead 
	                            ##re-calculates
		x <<- y					
		z <<- NULL
	}
	get_matrix <- function() { ##get_matrix returns x = matrix()
		x
	}
	set_solve <- function(inverse) {##set_solve changes the value of z to the inverse matrix calculated later in cacheSolve and 
	                                ##stores it as z. This information is now cached and can be re-used in later iterations
	                                ##without re-calculating
		z <- inverse			
	}
	get_solve <- function() {##get_solve returns the inverse matrix calculated in cacheSolve
		z
	}
	list(set_matrix = set_matrix, get_matrix = get_matrix, set_solve = set_solve, get_solve = get_solve)
}

##cacheSolve takes makeCacheMatrix as an argument x. It then return the cached inverse matrix for makeCacheMatrix, or 
##calculates a new if the matrix was changed

cacheSolve <- function(x, ...) { #cacheSolve takes makeCacheMatrix as an argument x
	z <- x$get_solve() ##calls get_solve from makeCacheMatrix (which is x). This takes the z-value from makeCacheMatrix and 
	                   ##stores it in a z-value in cacheSolve
	if(!is.null(z)) { ##if z is not null, the cached inverse matrix is returned
		message("getting cached data")
		return(z)
	}
	data_solve <- x$get_matrix() ##if z is null, get_matrix is called from makeCacheMatrix to get the new matrix and store it in 
	                             ##data_solve
	z <- solve(data_solve, ...) ##finds the inverse matrix for the new matrix stored in data_solve
	x$set_solve(z) ##calls set_solve from makeCacheMatrix to store the new inverse matrix into z
	z ##returns the new inverse matrix
}
