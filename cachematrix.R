## compute an inverse of squared matrix
## functions do

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  M<-NULL
  set<-function(z){
    x<<-z
    M<<-NULL
  }
  get<-function() x
 # setinverse <- aperm(matrix)*(1/det(matrix))
  setinverse <- solve(x)
  getinverse <- function() M
  list(set = set, get = get,
       setinverse= setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
         M<-x$getinverse() 
 if(det(x)!= 0){message("getting cached matrix inverse")
   return(M)}
 data <- x$get()
 M <- solve(data,...)
x$setinverse(M)
 M
        ## Return a matrix that is the inverse of 'x'
}
