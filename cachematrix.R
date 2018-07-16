#Inverse Matrix

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#Creates main assigned matrix and it's inverse as variables in the object instantiating
# with NULL
#Assigns each matrix respectively
makeCacheMatrix <- function(x = matrix()) 
  {
  inverse <- NULL
  assign <- function(s) {
    x <<- s
    inverse <<- NULL
  }
  get <- function() {
    print(x)
  }
  
  assign.inverse <- function(input){
    inverse <<- input
  }
  get.inverse <- function(){
    print(inverse)
  }
  list(assign = assign, get = get,
       assign.inverse = assign.inverse,
       get.inverse = get.inverse)
}

## Function creates inverse object that stores matrix's inverse if it is already there
# Else it solves for it and assigns it to the inverse property of the matrix

cacheSolve <- function(x, ...) {
  inverse <- x$get.inverse()
  if(is.null(inverse)){
    matrix <- x$get()
    inverse <- solve(matrix)
    x$assign.inverse(inverse)
  }else{
    return(inverse())
  }
  print(inverse)
}

#test
q<-matrix(c(2,0,0,1),2,2)
qz<-makeCacheMatrix(q)
cacheSolve(qz)
