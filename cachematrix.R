## makeCacheMatrix and cacheSolve work together to either fetch a cached version of an invertible matrix,
##  or work to solve the matrix and cache the solution to be fetched as required.

## makeCacheMatrix takes a matrix, x, and returns a list containing the an initialised value for the inverse of the matrix, . invmat is the inverse of the matrix, initialised as NULL.

makeCacheMatrix <- function(x = matrix()) {
      ##invmat, the inverse of the matrix, is initialised to zero, for when the makeCacheMatrix is called.
      invmat<-NULL
      set<-function(y){
        x<<-y
        invmat<<- NULL
      }
      ## get returns the value of the matrix, x. setinv is a function to solve the inverse of a matrix, getinv returns the value ofthe inverse.
      ##Note. setinv is called from cacheSolve.
      get <- function() x
      setinv<-function(solve) invmat <<- solve
      getinv<-function() invmat
      list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## cacheSolve takes a list, x, (created in makeCacheMatrix) and returns the inverse of the original matrix either by fetching the cached solution, or by solving and then caching.

cacheSolve <- function(x, ...) {
        
  invmat<-x$getinv()
  ##invmat is the inverse of the matrix and the following if statement checks to see if the value for invmat in the list created by makeCacheMatrix is null. If it isn't then the inverse is fetched and a message printed to the screen informing the user that this is cached data.
  if(!is.null(invmat)){
    message("getting cached data")
    return(invmat)
  }
  ##If the if statement above returns null as the value for invmat, then the following computes the inverse (solve) and stores this information in the cache.
  data <-x$get()
  invmat<-solve(data, ...)
  x$setinv(invmat)
  invmat
}
