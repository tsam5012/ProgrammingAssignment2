## Code that sets and gets the values of a matrix
## Then sets and gets the values of the inverse of the matrix

## This function creates a special matrix which is a list containing functions to get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {   #initialize matrix function
  mata <- NULL
  set <- function(y) {    #set value 
    x<<-y
    mata <<-NULL
  }
  get <- function() x     # get value 
  setinvmat<- function(inv) mata <<-inv   #set value 
  getinvmat <- function() mata            #get value 
  list(set=set,get=get, setinvmat=setinvmat,getinvmat=getinvmat)
}


## Function calculates inverse of special matrix: the function checks to see if the inverse was calculated
## if function has been calculated it returns the inverse from cache and skips calculation.
## Otherwise the function calculates the inverse of the matrix

cacheSolve <- function(x, ...) {
  mata <- x$getinvmat()
  if(!is.null(mata)){
    message("getting cached data")
    return(mata)
  }
  data <-x$get()
  mata<- solve(data,...)
  x$setinvmat(mata)
  mata
  ## Return a matrix that is the inverse of 'x'
}
