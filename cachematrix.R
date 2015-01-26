## makeCacheMatrix will create a matrix object and return list

makeCacheMatrix <- function(x = matrix()) {
  
  if("MASS" %in% rownames(installed.packages()) == FALSE){
    message("Install MASS....")
    install.packages("MASS")
    library("MASS")
  }
  
  invMax <- NULL
  set <- function(y) {
    x <<- y
    invMax <<- NULL
  }
  get <- function() x
  setInv <- function(inv) invMax <<- inv
  getInv <- function() invMax
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## cacheSolve will calculate and set the inverse matrix value 

cacheSolve <- function(x, ...) {
  
  if("MASS" %in% rownames(installed.packages()) == FALSE){
    message("Install MASS.....")
    install.packages("MASS")
    
  }  
  library("MASS")
  invMax <- x$getInv()
  
  if(!is.null(invMax)) {
    message("getting cached data")
    return(invMax)
  }
  
  data <- x$get()
  invMax <- ginv(data)
  x$setInv(invMax)
  
  invMax
}