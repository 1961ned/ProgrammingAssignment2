#Calculating inverse of matrices the quick way, using cached memory


makeCacheMatrix <- function(x = matrix()) {
  
    inv = NULL #since we assume matrix is invertible
    set = function(y) {
      # variable for change environment change
      x <<- y
      inv <<- NULL #assume matrix has an inverse
    }
    get = function() x
    setinv = function(inverse) inv <<- inverse 
    getinv = function() inv
    
    list(set=set, get=get, setinv=setinv, getinv=getinv)
    
  ##  output of makeCacheMatrix() and the
  ## return: inverse of the given matrix  
    cacheSolve <- function(x, ...) {
    
    
    inv = x$getinv()
    
    
    if (!is.null(inv)){
      # # if the inverse has already been calculated get it from the cache 
      message("getting cached data")
      return(inv)
    }
    
    # otherwise, go thru the actual calculation 
    mat.data = x$get()
    inv = solve(mat.data, ...)
    x$setinv(inv) #set value of the calculated inverse
    
    inv
  }
  
  
}

