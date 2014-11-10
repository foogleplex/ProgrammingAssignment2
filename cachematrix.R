## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        ## Sets cached inverse to NULL when making a new CacheMatrix
        cached_inverse <- NULL
        
        ## Set and get functions store matrix as "x"
        set <- function(y) {
                x <<- y
                ## Clears cached inverse when setting a new matrix
                cached_inverse <<- NULL
        }
        get <- function() x
        
        set_inverse <- function(inverse) cached_inverse <<- inverse
        
        get_inverse <- function() cached_inverse
        
        ## makeCacheMatrix function returns list which refers to internal functions
        list(set = set,
             get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated, then cacheSolve retrieves the inverse from the cache,
## otherwise, it calculates the inverse using solve(), sets the cached inverse to the the calculated inverse,
## and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## Attempts to load cached inverse into internal variable
        inverse <- x$get_inverse()
        
        ## Returns cached inverse if inverse is stored, with message
        if(!is.null(inverse)) {
                message("getting cached inverse")
                return(inverse)
        }
        
        ## If inverse is not cached, then load the matrix and solve for inverse 
        matrix <- x$get()
        inverse <- solve(matrix, ...)
        
        ## Store solved inverse and return the inverse
        x$set_inverse(inverse)
        inverse
}


