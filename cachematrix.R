## Put comments here that give an overall description of what your
## functions do
## programming assignment #2 for cousera rprog-008 by dave
##  inverse of matrix is time consuming and it's good to store the inverse 
##    for future use so that we need to invert the same matrix only once 
##   makeCacheMatrix: store input matrix and its inverse and provide set and get operations
##   cacheSolve:  return inverse matrix if stored.otherwise call solve to find invese 
##  usage sample:
##    m1 <- matrix(1:4, 2, 2)
##    m1cached <-  makeCacheMatrix(m1)
##    im1 <- cacheSolve(m1cached)


## Write a short comment describing this function
## this function store inverse matrix of the input matrix x
##  this provides 4 operations: get and set input data: x
##                         get and set inverse matrix: im
makeCacheMatrix <- function(x = matrix()) {
  
  im <- NULL
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) im <<- inverse
  getInverse <- function() im
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## Write a short comment describing this function
## this function return the stored inverse matrix if aready solved
## else find the inverse matrix using solve()
##  befoe call solve(), check if input matrix is null or sqaure 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  im <- x$getInverse()
  if(!is.null(im)) {
    message("getting cached matrix inverse")
    return(im)
  }
  data <- x$get()
  
  if(is.null(data) )
  {
    message("Error: input matrix is empty!")
    return(im)
  }
  
  if (nrow(data) != ncol(data)  )
  {
    message("Error: input matrix is not sqaure!")
    return(im)   
  }
  
  im <- solve(data, ...)
  
  x$setInverse(im)
  im
}
