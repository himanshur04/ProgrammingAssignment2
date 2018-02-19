## makeCacheMatrix function creates and returns a list of functions
## which are used by cacheSolve to get/set the invert of the matrix (in cache) 

makeCacheMatrix <- function(x = matrix()) {
    #initializing the cache variable to NULL 
    p <- NULL
    
    # set/create a matrix in workign environment
    set <- function(y) {
            x <<- y
            p <<- NULL
    }
    get <- function() x
    # setMatrix will invert the matrix and stores it in cache (p)
    setMatrix <- function(inv) p <<- inv
    # retrives the inverted matrix from cache (p)
    getInv <- function() p
    
    #list containing created functions
    list(set = set, get = get,
         setMatrix = setMatrix,
         getInv = getInv)
}


## cacheSolve function calculates the inverse of the matrix created
## above. If the inverted matrix is not present, it will create it in
## in the workign environment and stores it in cache

cacheSolve <- function(x, ...) {
              # getInv get the Inverse of the matrix
              p <- x$getInv()
              
              # gets cached matrix if it exists else creates it
              if(!is.null(p)) {
                message("getting cached matrix")
                return(p)
              }
              
              # matrix creation
              matrix_hr <- x$get()
              #Solve function inverse the square matrix
              p <- solve(matrix_hr, ...)
              x$setMatrix(p)
              return(p)
}
