makeMatrix <- function(x = matrix()){
  stored_inverse <- NULL
  set <- function(y){
    x <<- y 
    stored_inverse <<- NULL
  }
  get <- function() x
  #create getters and setters
  setinverse <- function(inverse) stored_inverse <<- inverse #Store inverse in parent envoirement
  getinverse <- function() stored_inverse
  #Return a list of functions and variables
  list(set = set, get = get, setinverse = setinverse, getinverse= getinverse)
}

cacheInverse <- function(x, ...){
  inverse <- x$getinverse()
  # if inverse is not null, retrieve it from the parent scope.
  if(!is.null(inverse)){
    message("getting cashed inverse of matrix")
    return (inverse) #exit by returning inverse
  }
  data <- x$get()
  inverse <- solve(data, ...) # calculate inverse.
  x$setinverse(inverse) # store inverse is x. (x stores inverse in parent scope.)
  inverse # return inverse
}