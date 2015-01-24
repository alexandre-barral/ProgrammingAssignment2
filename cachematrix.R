## makeCacheMatrix function - contains a list of functions that return and get the inverse of a matrix
## cacheSolve function - evaluate and storage the inverse of a matrix

## makeCacheMatrix is a function with:
## input: x is a square invertible  matrix
## output: a list of functions, where:
##       get() - return 'x' matrix 
##	 set_inv() - cache the inverse of 'x' matrix
##	 get_inv() - return inverse of 'x'
makeCacheMatrix <- function(x = matrix()) {

	## always makeCacheMatrix is call set inv as NULL, this way when 'x' is changed the inverse is reset	
	inv <- NULL 

	## local functions		
	get <- function() x
	set_inv <- function(inv_) inv <<- inv_ 
	get_inv <- function() inv
	
	## return a list of function
	list(get = get, set_inv = set_inv, get_inv = get_inv)

}



## cacheSolve is a function with:
## input: x a makeCacheMatrix object
## output: the inverse of the matrix storage in x
cacheSolve <- function(x, ...) {

        ## gets the inverse of  'x'
	m_inv <- x$get_inv()
	
	## if inverse is cached return value cached
	if (!is.null(m_inv)) return(m_inv)

	## inverse not calculated yet, evaluate inverse
	m_inv <- solve(x$get())
	## set inverse in 'x'
	x$set_inv(m_inv)
	## return value of inverse
	m_inv
}
