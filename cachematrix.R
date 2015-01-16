## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
    inverse <- NULL                      ## reset inverse when function is called
    
    set <- function(y) {                 ## input matrix
      x <<- y                            ## save input matrix in 'x'
      inverse <<- NULL                   ## reset inverse 
    }
    
    get <- function() {x}                ## gets the original matrix
    setmatrix <- function(solve) {inverse <<- solve}  ## called by cacheSolve() at first cacheSolve
    getmatrix <- function() {inverse}                 ## returns cached value
    
    list(set = set, get = get,           ## list of internal functions so 'calling' function
         setmatrix = setmatrix,          ## knows how to access those functions
         getmatrix = getmatrix)          

}


## Write a short comment describing this function

cacheSolve <- function(x= matrix(), ...) {
                                         ## Return a matrix that is the inverse of 'x'
    
    inverse <- x$getmatrix()             ## access to the object 'x' and gets the inverse
    
    if (!is.null(inverse)) {             ## if inverse was already cached (not NULL)
      message("getting cached inverse")  ## print
      return(inverse)                    ## return the inverse to the function
    }
  
    data <- x$get()                      ## if x$getinverse() gives NULL
    inverse <- solve(data, ...)          ## calculate the inverse
    x$setmatrix(inverse)                 ## assign the value of inverse to 'x'
    inverse                              ## last statement of sub-function, returns value to calling function
  
}
