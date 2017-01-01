## makeCacheMatrix creates a matrix which is invertible

  makeCacheMatrix <- function(x = matrix()) {   ##makeCacheMatrix is a function
    s <- NULL                                   ## s is NULL
    set <- function(y) {                        ## set a function
      x <<- y
      s <<- NULL
    }
    get <- function() x                         ## get the requested function
    setsolve <- function(solve) s <<- solve     ## set the inverse of x matrix
    getsolve <- function() s                    ## get the inverse of x matrix
    list(set = set, get = get,                  ## list the functions above
         setsolve = setsolve,
         getsolve = getsolve)
  }
  
## cacheSolve solves the matrix x and prints and caches the inverse
            
  cacheSolve <- function(x, ...) {              ## cacheSolve is a function
    s <- x$getsolve()                           ## s is now either the inverse of x matrix or NULL
    if(!is.null(s)) {                           ## if s is not NULL...
      message("getting cached data")            ## ... we are told R is "getting cached data"
      return(s)                                 ## print s(x matrix inverse)
    }
          else{                                 ## if s is anything except not NULL
    data <- x$get()                             ## get x matrix and save as data
    s <- solve(data)                            ## solve data and save it as s
    x$setsolve(s)                               
    s                                           ## print s
  }                                             ## End function
